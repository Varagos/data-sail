import '@/styles/globals.css';
import {
  Address,
  Blockfrost,
  Lucid,
  MintingPolicy,
  PolicyId,
  ScriptHash,
  SpendingValidator,
  TxHash,
  UTxO,
  Unit,
} from 'lucid-cardano';
import type { AppProps } from 'next/app';
import { Dispatch, SetStateAction, createContext, useEffect, useState } from 'react';

const dataListingScript: SpendingValidator = {
  type: 'PlutusV2',
  script:
    '590c24590c21010000323232323232323322332232323232323232323232323322323232323232323232323232323322323223232232232325335323232325335003153355335333573466e20c8c0a8004ccd54c0384800488cd54c04c480048d400488cd540dc008cd54c058480048d400488cd540e8008ccd40048cc0e52000001223303a00200123303900148000004cd54c04c480048d400488cd540dc008ccd40048cd54c05c480048d400488cd540ec008d5406400400488ccd5540500740080048cd54c05c480048d400488cd540ec008d54060004004ccd55403c06000800540b8c8c8d4004888888888888ccd54c0704800488d40088888d401088cd400894cd4ccd5cd19b8f017001043042133504400600810082008503c00a50023500422002350042200102b02c102c1335738920121416d6f756e74207265717569726564206e6f74207061696420746f206f776e65720002b15335333573466e1cc8cccd54c04c48004c8cd405c88ccd406000c004008d4054004cd4058888c00cc008004800488cdc0000a40040029000199aa98070900091299a99a801101711980c8009aa99a9a802911a8011111111111111999a8069281812818128181199aa98120900099a81391299a801108018800a81811a80091299aa99a999ab9a3371e6a004440046a008440040820802666ae68cdc39a801110009a80211000820820082009a81a0018a819806909a800911a80091111a8021119a8011181f24c466aa07e00200a260689311001099a8178010008800a81719aa980b8900091a9a9a800910009111002110011aa800911111111111006240040580562058266ae712411e596f75206d75737420636f6e73756d65206f6e6c79206f6e65207574786f0002b102b13263203433573892011652656465656d206e6f7420696d706c656d656e74656400034135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4094098d5d0a80619a8128131aba1500b33502502735742a014666aa052eb940a0d5d0a804999aa814bae502835742a01066a04a0606ae85401cccd540a40c5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40edd69aba15002303c357426ae8940088c98c8124cd5ce01e82482389aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81dbad35742a00460786ae84d5d1280111931902499ab9c03d049047135573ca00226ea8004d5d09aba2500223263204533573807208a08626aae7940044dd50009aba1500533502575c6ae854010ccd540a40b48004d5d0a801999aa814bae200135742a004605e6ae84d5d1280111931902099ab9c03504103f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008603e6ae84d5d1280211931901999ab9c0270330313333573466e1d40152002212200223333573466e1d40192000212200123263203333573804e0660620606666ae68cdc39aab9d5007480008cc8848cc00400c008dd71aba15007375a6ae84d5d1280391931901899ab9c02503102f10301326320303357389210350543500030135573ca00226ea80044d55ce9baa001135573ca00226ea8004444888ccd54c01048005408ccd54c01c480048d400488cd540ac008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd404088ccd400c88008008004d40048800448cc004894cd400840a440040988d400488cc028008014018400c4cd409c01000d4090004cd54c01c480048d400488c8cd540b000cc004014c8004d540b8894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5409c88448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540908844894cd400454084884cd4088c010008cd54c01848004010004c8004d5408c88448894cd40044d401800c884ccd4024014c010008ccd54c01c4800401401000448d40048800448d40048800848848cc00400c00888ccd5cd19b8f0020010180173200135501e22112253350011501b22133501c300400233553006120010040011232230023758002640026aa03c446666aae7c004940688cd4064c010d5d080118019aba200201f232323333573466e1cd55cea8012400046644246600200600460186ae854008c014d5d09aba2500223263201f33573802603e03a26aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602a6ae854008cd4034050d5d09aba2500223263202433573803004804426aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931901319ab9c01a026024023022135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c8080cd5ce00a01000f09aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5406c88c8cccd55cf8011280c119a80b99aa80e18031aab9d5002300535573ca00460086ae8800c0744d5d080089119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6403a66ae7004407406c0684d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6403666ae7003c06c06406005c0584d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6402e66ae7002c05c0544d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263201533573801202a02626ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201e33573802403c03803603403203002e02c26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900b99ab9c00b017015014135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8050cd5ce00400a00900889aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900a99ab9c009015013012011135573aa00226ea80048c8cccd5cd19b8750014800880208cccd5cd19b8750024800080208c98c8044cd5ce00280880780709aab9d375400292103505431003200135500c223350014800088d4008894cd4ccd5cd19b8f00200c00900813007001130060033200135500b223350014800088d4008894cd4ccd5cd19b8f00200b0080071001130060031220021220011122002122122330010040034881002233700004002464c6400866ae712401024c67000041122123300100300249848004448c8c00400488cc00cc0080080041',
};

export type AppState = {
  // Global
  lucid?: Lucid;
  wAddr?: Address;

  dataTokenPolicyIdHex?: PolicyId;
  dataTokenNameHex?: string;
  dataTokenAssetClassHex?: Unit;
  dataTokenPolicy?: MintingPolicy;
  dataListingScript: SpendingValidator;
};

const initialAppState: AppState = {
  dataListingScript: dataListingScript,
};

export const AppStateContext = createContext<{
  appState: AppState;
  setAppState: Dispatch<SetStateAction<AppState>>;
}>({ appState: initialAppState, setAppState: () => {} });

export default function App({ Component, pageProps }: AppProps) {
  const [appState, setAppState] = useState<AppState>(initialAppState);

  const connectLucidAndNami = async () => {
    const blockFrostKey = process.env.NEXT_PUBLIC_BLACKFROST_KEY;
    // console.log({ blockFrostKey });
    const lucid = await Lucid.new(
      new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blockFrostKey),
      'Preview'
    );
    if (!window.cardano.nami) {
      window.alert('Please install Nami Wallet');
      return;
    }
    const nami = await window.cardano.nami.enable();
    lucid.selectWallet(nami);
    setAppState({
      ...initialAppState,
      lucid: lucid,
      wAddr: await lucid.wallet.address(),
    });
  };

  useEffect(() => {
    if (appState.lucid) return;
    connectLucidAndNami();
  }, [appState]);

  return (
    <AppStateContext.Provider value={{ appState, setAppState }}>
      <Component {...pageProps} />
    </AppStateContext.Provider>
  );
}
