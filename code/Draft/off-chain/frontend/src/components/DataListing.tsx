import React, { useContext, useEffect, useState } from 'react';
import CryptoJS from 'crypto-js';
import ButtonDarkFullWidth from './elements/dark-full-w-button';
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
import { AppStateContext } from '@/pages/_app';
import { signAndSubmitTx } from '@/utilities/utilities';
import usePollingData from '@/hooks/usePollingData';

const COMPILED_MINTING_VALIDATOR_WITHOUT_PARAMS =
  '5909065909030100003233223322323232323232323232323232323232323232323232222223232533532323232325335533533355300d12001323212330012233350052200200200100235001220011233001225335002102610010232325335333573466e3cd400488008d401c880080940904ccd5cd19b87350012200135007220010250241024350012200235500122222222222200c10231335738921115554784f206e6f7420636f6e73756d65640002215335533532330215026001355001222222222222008102222135002222533500415335333573466e3c0080240a009c4ccd5cd19b87001480080a009c409c8840a4408c4cd5ce24811357726f6e6720616d6f756e74206d696e746564000221022135001220023333573466e1cd55cea802a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406c070d5d0a80619a80d80e1aba1500b33501b01d35742a014666aa03eeb94078d5d0a804999aa80fbae501e35742a01066a03604a6ae85401cccd5407c099d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40c1d69aba150023031357426ae8940088c98c80cccd5ce01a01981889aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8183ad35742a00460626ae84d5d1280111931901999ab9c034033031135573ca00226ea8004d5d09aba2500223263202f33573806005e05a26aae7940044dd50009aba1500533501b75c6ae854010ccd5407c0848004d5d0a801999aa80fbae200135742a00460486ae84d5d1280111931901599ab9c02c02b029135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00a60266ae84d5d1280291931900e99ab9c01e01d01b3333573466e1cd55ce9baa0064800080708c98c8070cd5ce00e80e00d1bae00633011375c00e6eb401840644c98c8064cd5ce2490350543500019135573ca00226ea8004c8004d5406488448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80c911999aab9f0012501b233501a30043574200460066ae8800804c8c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c804ccd5ce00a00980889aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a1aba1500233500d013357426ae8940088c98c8060cd5ce00c80c00b09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403466ae7006c06806005c0584d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201433573802a02802426ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a032466a03066442466002006004600c6aae754008c014d55cf280118021aba200301113574200224464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900899ab9c01201100f00e135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c020d5d09aab9e500323333573466e1d40092004232122223002005300a357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01201100f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011980298031aba15002375a6ae84d5d1280111931900699ab9c00e00d00b135573ca00226ea80048848cc00400c0088c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200a23333573466e1d40092000200a23263200633573800e00c00800626aae74dd5000a4c24002921035054310032001355006222533500110022213500222330073330080020060010033200135500522225335001100222135002225335333573466e1c005200000a0091333008007006003133300800733500b12333001008003002006003122002122001112200212212233001004003112323001001223300330020020011';

// const dataListingScript: SpendingValidator = {
//   type: 'PlutusV2',
//   script:
//     '590aa6590aa30100003232323233223322323232323232323232323232323232323232323232323322323223232232232325335323232533500215335333573466e20c8c094004ccd54c0344800488cd54c048480048d400488cd540bc008cd54c054480048d400488cd540c8008ccd40048cc0d52000001223303600200123303500148000004cd54c048480048d400488cd540bc008ccd40048cd54c058480048d400488cd540cc008d5406000400488ccd55404c0640080048cd54c058480048d400488cd540cc008d5405c004004ccd55403805000800540a4c8c8d4004888888888888ccd54c06c4800488d40088888d401088cd400894cd4ccd5cd19b8f01700103e03d133503f00600810082008503700a35002220023500322002350032200102602710271335738920121616d6f756e74207265717569726564206e6f74207061696420746f206f776e65720002613263202233573892011652656465656d206e6f7420696d706c656d656e746564000223333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4084088d5d0a80619a8108111aba1500b33502102335742a014666aa04aeb94090d5d0a804999aa812bae502435742a01066a0420546ae85401cccd540940add69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40d5d69aba150023036357426ae8940088c98c80e0cd5ce01c81c01b09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81abad35742a004606c6ae84d5d1280111931901c19ab9c039038036135573ca00226ea8004d5d09aba2500223263203433573806a06806426aae7940044dd50009aba1500533502175c6ae854010ccd5409409c8004d5d0a801999aa812bae200135742a00460526ae84d5d1280111931901819ab9c03103002e135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860326ae84d5d1280211931901119ab9c0230220203333573466e1d40152002212200223333573466e1d40192000212200123263202233573804604404003e6666ae68cdc39aab9d5007480008cc8848cc00400c008dd71aba15007375a6ae84d5d1280391931901019ab9c02102001e101f13263201f3357389201035054350001f135573ca00226ea80044d55ce9baa001135573ca00226ea8004444888ccd54c01048005407ccd54c01c480048d400488cd54090008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8c848cc00488ccd401488008008004008d40048800448cc004894cd4008409440040888d400488cc028008014018400c4cd408c01000d4080004cd54c01c480048d400488c8cd5409400cc004014c8004d5409c894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5408088448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540748844894cd400454074884cd4078c010008cd54c01848004010004c8004d5407088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f0020010170161232230023758002640026aa036446666aae7c004940688cd4064c010d5d080118019aba2002012232323333573466e1cd55cea8012400046644246600200600460146ae854008c014d5d09aba2500223263201233573802602402026aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460266ae854008cd4034048d5d09aba2500223263201733573803002e02a26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900c99ab9c01a019017016015135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c804ccd5ce00a00980889aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5406088c8cccd55cf8011280c119a80b99aa80c98031aab9d5002300535573ca00460086ae8800c0404d5d080089119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6402066ae700440400380344d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b8750014800880288cccd5cd19b8750024800080288c98c8018cd5ce00380300200189aab9d37540029309000a481035054310032001355009223350014800088d4008894cd4ccd5cd19b8f00200d009008130070011300600332001355008223350014800088d4008894cd4ccd5cd19b8f00200c0080071001130060031220021220011122002122122330010040031122123300100300248900223370000400222464600200244660066004004003',
// };

export const DataListingRedeemer = Data.Enum([Data.Literal('Redeem'), Data.Literal('Purchase')]);
export type DataListingRedeemerType = Data.Static<typeof DataListingRedeemer>;

export const DataListingDatumSchema = Data.Object({
  dataSeller: Data.Bytes(),
  price: Data.Integer(),

  // dataSellerAddress: Data.Bytes(),
  // In lovelace
  // dataLocation: Data.Bytes(),
});
export type DataListingDatumType = Data.Static<typeof DataListingDatumSchema>;
export const DataListingDatum = DataListingDatumSchema as unknown as DataListingDatumType;

function DataListing() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex, dataListingScript } = appState;
  const [randomData, setRandomData] = useState('');
  const [encryptedData, setEncryptedData] = useState('');
  const [encryptionKey, setEncryptionKey] = useState('No Key');
  // Poll every 3s
  const data = usePollingData(3000);
  const [askingPrice, setAskingPrice] = useState<number | null>(null);

  /**
   * 1. Encrypt data
   * 2. Transaction to mint DataToken and lock under DataListing smart contract
   */
  const handleDataSelling = async () => {
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

    await mintDataToken();
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
    // Convert to hex
    const tokenNameHex = fromText('DataToken');
    // TransactionId, txIndex and tokenName
    const ParamsSchema = Data.Tuple([Data.Bytes(), Data.Integer(), Data.Bytes()]);
    type Params = Data.Static<typeof ParamsSchema>;
    const Params = ParamsSchema as unknown as Params;
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

  // TODO Add check that we don't already have a DataToken in the wallet
  const mintDataToken = async () => {
    console.log(`Minting DataToken for ${wAddr}`);
    if (!wAddr) throw new Error('Wallet not initialized');
    if (!lucid) throw new Error('Lucid not initialized');
    const utxo = await getUtxo(wAddr);
    console.log(`Found UTxO: ${utxo.address}`);
    const { policy, unit } = getFinalPolicy(utxo);

    // lucid.utils.keyHashToCredential
    //   .payToAddress
    // const a = lucid.utils.keyHashToCredential('ad');
    // const address = lucid.utils.credentialToAddress(a);
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

  const lookWalletForDataToken = async (): Promise<UTxO | null> => {
    if (!lucid) throw new Error('Lucid not initialized');
    if (!wAddr) throw new Error('Wallet not initialized');
    const utxos = await lucid.utxosAt(wAddr);
    const tokenNameHex = fromText('DataToken');

    const dataTokenUtxo = utxos.find((utxo) => {
      const assetIds = Object.keys(utxo.assets);
      if (assetIds.length === 0) return false;
      return assetIds.some((assetId) => assetId.endsWith(tokenNameHex));
    });

    return dataTokenUtxo ?? null;
  };
  /**
   * TODO? merge into 1 tx with minting of token?
   */
  const lockUnderDataListing = async (nftUtxo: UTxO | null) => {
    console.log({ askingPrice });
    const { dataTokenAssetClassHex: nftAssetClassHex } = appState;
    if (!nftAssetClassHex) {
      throw new Error('DataToken not minted');
      // const dataTokenUtxo = await lookWalletForDataToken();
    }
    if (!lucid) throw new Error('Lucid not initialized');
    if (!wAddr) throw new Error('Wallet not initialized');

    const dataListingAddr = lucid.utils.validatorToAddress(dataListingScript);
    console.log(`Locking DataToken under DataListing ${dataListingAddr}`);

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || '';

    const datum: DataListingDatumType = {
      dataSeller: pkh,
      // probably need to use fromHex here
      // dataSellerAddress: fromText(wAddr),
      price: BigInt(1_000_000),
      // dataOwner: '123',
      // dataLocation: '123',
    };
    console.log('created datum');

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
    const txId = await signAndSubmitTx(tx);
    console.log(`DataToken locked tx: ${txId}`);
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <ul className="w-full p-4">
          {(data?.[0] ?? []).map((item, index) => (
            <li key={index} className="flex justify-between py-2 border-b">
              <a href={item.url} target="_blank" rel="noopener noreferrer" className="text-blue-600 truncate">
                {item.url}
              </a>
              <span className="text-zinc-800">Visits: {item.count}</span>
            </li>
          ))}
        </ul>
        {/* <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={generateData}>Generate Random Data</ButtonDarkFullWidth>
          <p className="my-2">{randomData}</p>
        </div> */}
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={handleDataSelling}>Mint Data Token</ButtonDarkFullWidth>
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
        {/* Add an input for asking price */}
        <div className="flex flex-col mb-2">
          <input
            type="text"
            placeholder="Asking Price"
            className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            onChange={(e) => setAskingPrice(parseInt(e.target.value))}
          />
        </div>
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={() => lockUnderDataListing(null)}>List Data for Sale</ButtonDarkFullWidth>
        </div>
      </div>
    </div>
  );
}

export default DataListing;
