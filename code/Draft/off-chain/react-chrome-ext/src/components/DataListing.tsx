import React, { useContext, useState } from 'react';
import { sentence } from 'txtgen';
import CryptoJS from 'crypto-js';
import ButtonDarkFullWidth from './elements/dark-full-w-button';
import { storage } from '../utilities/storage';
import {
  Data,
  MintingPolicy,
  PolicyId,
  SpendingValidator,
  UTxO,
  Unit,
  applyParamsToScript,
  fromText,
  getAddressDetails,
} from 'lucid-cardano';
import { signAndSubmitTx } from '../utilities/utilities';
import { AppStateContext } from '../context/AppStateContext';

const COMPILED_MINTING_VALIDATOR_WITHOUT_PARAMS =
  '5909065909030100003233223322323232323232323232323232323232323232323232222223232533532323232325335533533355300d12001323212330012233350052200200200100235001220011233001225335002102610010232325335333573466e3cd400488008d401c880080940904ccd5cd19b87350012200135007220010250241024350012200235500122222222222200c10231335738921115554784f206e6f7420636f6e73756d65640002215335533532330215026001355001222222222222008102222135002222533500415335333573466e3c0080240a009c4ccd5cd19b87001480080a009c409c8840a4408c4cd5ce24811357726f6e6720616d6f756e74206d696e746564000221022135001220023333573466e1cd55cea802a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406c070d5d0a80619a80d80e1aba1500b33501b01d35742a014666aa03eeb94078d5d0a804999aa80fbae501e35742a01066a03604a6ae85401cccd5407c099d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40c1d69aba150023031357426ae8940088c98c80cccd5ce01a01981889aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8183ad35742a00460626ae84d5d1280111931901999ab9c034033031135573ca00226ea8004d5d09aba2500223263202f33573806005e05a26aae7940044dd50009aba1500533501b75c6ae854010ccd5407c0848004d5d0a801999aa80fbae200135742a00460486ae84d5d1280111931901599ab9c02c02b029135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00a60266ae84d5d1280291931900e99ab9c01e01d01b3333573466e1cd55ce9baa0064800080708c98c8070cd5ce00e80e00d1bae00633011375c00e6eb401840644c98c8064cd5ce2490350543500019135573ca00226ea8004c8004d5406488448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80c911999aab9f0012501b233501a30043574200460066ae8800804c8c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c804ccd5ce00a00980889aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a1aba1500233500d013357426ae8940088c98c8060cd5ce00c80c00b09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403466ae7006c06806005c0584d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201433573802a02802426ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a032466a03066442466002006004600c6aae754008c014d55cf280118021aba200301113574200224464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900899ab9c01201100f00e135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c020d5d09aab9e500323333573466e1d40092004232122223002005300a357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01201100f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011980298031aba15002375a6ae84d5d1280111931900699ab9c00e00d00b135573ca00226ea80048848cc00400c0088c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200a23333573466e1d40092000200a23263200633573800e00c00800626aae74dd5000a4c24002921035054310032001355006222533500110022213500222330073330080020060010033200135500522225335001100222135002225335333573466e1c005200000a0091333008007006003133300800733500b12333001008003002006003122002122001112200212212233001004003112323001001223300330020020011';

const dataListingScript: SpendingValidator = {
  type: 'PlutusV2',
  script:
    '590a12590a0f010000323233223322323232323232323232323232323232323232323232322323232232232325335323232533500215335333573466e20c8c088004d54cd4ccd54c03448004894cd4c8c8d400888d400c88c8cd40148cd401094cd4ccd5cd19b8f00200103103015003103020302335004203025335333573466e3c0080040c40c05400c40c054cd400c854cd400884cd40088cd40088cd40088cd40088cc07000800480cc8cd400880cc8cc0700080048880cc888cd401080cc8894cd4ccd5cd19b8700600303603515335333573466e1c0140080d80d44ccd5cd19b8700400103603510351035102e153350012102e102e3500622200335002222200413350250020011001502435350012200222222222222200a13263201f3357389211d4e6f206f757470757420746f20636f6c6c61746572616c206f776e65720001f22153350011002221326320233357389201284d6f7265207468616e206f6e65206f757470757420746f20636f6c6c61746572616c206f776e65720002322220033500322200202502610261335738920121616d6f756e74207265717569726564206e6f74207061696420746f206f776e65720002513263201f33573892011652656465656d206e6f7420696d706c656d656e7465640001f3333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4074078d5d0a80619a80e80f1aba1500b33501d01f35742a014666aa042eb94080d5d0a804999aa810bae502035742a01066a03a04e6ae85401cccd540840a1d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40c9d69aba150023033357426ae8940088c98c80d4cd5ce01b01a81989aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8193ad35742a00460666ae84d5d1280111931901a99ab9c036035033135573ca00226ea8004d5d09aba2500223263203133573806406205e26aae7940044dd50009aba1500533501d75c6ae854010ccd540840908004d5d0a801999aa810bae200135742a004604c6ae84d5d1280111931901699ab9c02e02d02b135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008602c6ae84d5d1280211931900f99ab9c02001f01d3333573466e1d40152002212200223333573466e1d40192000212200123263201f33573804003e03a0386666ae68cdc39aab9d5008480008ccc88848ccc00401000c008c048d5d0a8041bad35742a00e6eb8d5d09aba2500723263201d33573803c03a0362038264c6403866ae71241035054350001c135573ca00226ea80044d55ce9baa001135744a00226aae7940044dd5000990009aa80d9108911299a800880111099802801199aa98038900080280200091199ab9a3371e0040020340322464460046eb0004c8004d5406888cccd55cf8009280c919a80c18021aba1002300335744004026464646666ae68cdc39aab9d5002480008cc8848cc00400c008c02cd5d0a80118029aba135744a004464c6402666ae7005004c0444d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a00800600460126ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403466ae7006c06806005c0584d55cea80089baa00135742a00466a014eb8d5d09aba2500223263201433573802a02802426ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355017223233335573e0044a02e466a02c66442466002006004600c6aae754008c014d55cf280118021aba2003011135742002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c02cd5d0a80119a8028051aba135744a004464c6401e66ae7004003c0344d55cf280089baa00112232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8040cd5ce00880800700689aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00880800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00680600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae7002c0280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005004c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801a01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00a009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801601401000e00c26aae7540044dd50009191999ab9a3370ea0029001100611999ab9a3370ea0049000100611931900319ab9c007006004003135573a6ea800526120014901035054310032001355007223350014800088d4008894cd4ccd5cd19b8f00200c00b00a130070011300600332001355006223350014800088d4008894cd4ccd5cd19b8f00200b00a009100113006003112200212212233001004003122002122001488100112323001001223300330020020011',
};

const DataListingRedeemer = Data.Enum([Data.Literal('Redeem'), Data.Literal('Purchase')]);
type DataListingRedeemer = Data.Static<typeof DataListingRedeemer>;

const DataListingDatum = Data.Object({
  dataOwner: Data.Bytes(),
  price: Data.Integer(),
  dataLocation: Data.Bytes(),
});
type DataListingDatum = Data.Static<typeof DataListingDatum>;

function DataListing() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex } = appState;
  const [randomData, setRandomData] = useState('');
  const [encryptedData, setEncryptedData] = useState('');
  const [encryptionKey, setEncryptionKey] = useState('No Key');

  const generateData = () => {
    setRandomData(sentence());
  };

  /**
   * 1. Encrypt data
   * 2. Transaction to mint DataToken and lock under DataListing smart contract
   */
  const handleDataSelling = () => {
    /***
     * CryptoJS.lib.WordArray.random(128/8) generates a 128-bit random encryption key.
     * The .toString(CryptoJS.enc.Hex) converts the key to a hex string for easier handling.
     */
    const key = CryptoJS.lib.WordArray.random(128 / 8).toString(CryptoJS.enc.Hex);
    setEncryptionKey(key);
    const ciphertext = CryptoJS.AES.encrypt(randomData, key).toString();
    setEncryptedData(ciphertext);
    // Here you can store the ciphertext wherever you want
    console.log({ encryptedData: ciphertext });
    storage.storeData(ciphertext);

    mintDataToken();
    decryptData(ciphertext, key);
  };

  const hideKey = () => {
    setEncryptionKey('No Key');
  };

  const decryptData = (encryptedData: string, encryptionKey: string) => {
    const bytes = CryptoJS.AES.decrypt(encryptedData, encryptionKey);
    const originalData = bytes.toString(CryptoJS.enc.Utf8);

    console.log(originalData); // log the decrypted data to console, or use it however you wish
    return originalData;
  };

  const getUtxo = async (address: string): Promise<UTxO> => {
    if (!lucid) throw new Error('Lucid not initialized');
    const utxos = await lucid.utxosAt(address);
    if (utxos.length === 0) throw new Error('No UTxOs found');
    return utxos[0];
  };

  type GetFinalPolicy = {
    policy: MintingPolicy;
    // asset-class
    unit: Unit;
  };

  // Apply the params
  const getFinalPolicy = (utxo: UTxO): GetFinalPolicy => {
    const tokenNameHex = fromText('DataToken');
    // TransactionId, txIndex and tokenName
    const Params = Data.Tuple([Data.Bytes(), Data.Integer(), Data.Bytes()]);
    type Params = Data.Static<typeof Params>;
    const dataTokenPolicy: MintingPolicy = {
      type: 'PlutusV2',
      script: applyParamsToScript<Params>(
        // the compiled validator, without the parameters applied
        COMPILED_MINTING_VALIDATOR_WITHOUT_PARAMS,
        [utxo.txHash, BigInt(utxo.outputIndex), tokenNameHex],
        Params
      ),
    };
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(dataTokenPolicy);
    const tokenAssetClass: Unit = policyId + tokenNameHex;
    setAppState((appState) => ({
      ...appState,
      dataTokenPolicyIdHex: policyId,
      dataTokenNameHex: tokenNameHex,
      dataTokenAssetClassHex: tokenAssetClass,
      dataTokenPolicy: dataTokenPolicy,
    }));

    return { policy: dataTokenPolicy, unit: tokenAssetClass };
  };

  const mintDataToken = async () => {
    console.log(`Minting DataToken for ${wAddr}`);
    if (!wAddr) throw new Error('Wallet not initialized');
    if (!lucid) throw new Error('Lucid not initialized');
    const utxo = await getUtxo(wAddr);
    const { policy, unit } = getFinalPolicy(utxo);

    const redeemer = Data.void();
    const tx = await lucid
      .newTx()
      .mintAssets({ [unit]: 1n }, redeemer)
      .attachMintingPolicy(policy)
      // Collect from the Token parameterized UTxO
      .collectFrom([utxo])
      // Balance the tx
      .complete();

    await signAndSubmitTx(tx);
  };

  const lockUnderDataListing = async (nftAssetClassHex: string, nftUtxo: UTxO) => {
    if (!lucid) throw new Error('Lucid not initialized');
    if (!wAddr) throw new Error('Wallet not initialized');

    const dataListingAddr = lucid.utils.validatorToAddress(dataListingScript);
    console.log(`Locking DataToken under DataListing ${dataListingAddr}`);

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || '';

    const datum: DataListingDatum = {
      dataOwner: '123',
      price: BigInt(1000000),
      dataLocation: '123',
    };

    const tx = await lucid!
      .newTx()
      // .collectFrom([nftUtxo]) // Not needed since NFT is already in the wallet
      .payToContract(
        dataListingAddr,
        // Datum of type Integer
        { inline: Data.to(datum, DataListingDatum) },
        // the value we pay is the nft.
        { [nftAssetClassHex]: 1n }
      )
      .addSignerKey(pkh)
      .complete();
    await signAndSubmitTx(tx);
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={generateData}>Generate Random Data</ButtonDarkFullWidth>
          <p className="my-2">{randomData}</p>
        </div>
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={handleDataSelling}>List Data for Sale</ButtonDarkFullWidth>
          <div className="flex justify-between items-center mt-2">
            <p className="my-2 flex-grow">{encryptionKey}</p>
            <button
              onClick={hideKey}
              className="w-16 rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              Hide Key
            </button>
          </div>
        </div>
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={() => {}}>List Data for Sale</ButtonDarkFullWidth>
        </div>
      </div>
    </div>
  );
}

export default DataListing;
