// import {
//   Address,
//   Blockfrost,
//   Lucid,
//   MintingPolicy,
//   PolicyId,
//   ScriptHash,
//   SpendingValidator,
//   TxHash,
//   UTxO,
//   Unit,
// } from 'lucid-cardano';
// import React, { createContext, useEffect, useState } from 'react';

// export type AppState = {
//   lucid?: Lucid;
//   wAddr?: Address;
//   // Your AppState type here
//   dataTokenPolicyIdHex?: any;
// };

// const initialAppState: AppState = {};

// export const AppStateContext: React.Context<{
//   appState: AppState;
//   setAppState: React.Dispatch<React.SetStateAction<AppState>>;
// }> = createContext<{
//   appState: AppState;
//   setAppState: React.Dispatch<React.SetStateAction<AppState>>;
// }>({ appState: initialAppState, setAppState: () => {} });

// export default function AppStateProvider({ Component, pageProps }: any) {
//   const [appState, setAppState] = useState<AppState>(initialAppState);

//   const connectLucidAndNami = async () => {
//     const blackFrostKey = process.env.REACT_APP_BLACKFROST_KEY;
//     console.log({ blackFrostKey });
//     const lucid = await Lucid.new(
//       new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blackFrostKey),
//       'Preview'
//     );
//     if (!window.cardano.nami) {
//       window.alert('Please install Nami Wallet');
//       return;
//     }
//     const nami = await window.cardano.nami.enable();
//     lucid.selectWallet(nami);
//     setAppState({
//       ...initialAppState,
//       lucid: lucid,
//       wAddr: await lucid.wallet.address(),
//     });
//   };

//   useEffect(() => {
//     if (appState.lucid) return;
//     connectLucidAndNami();
//   }, [appState]);
//   return (
//     <AppStateContext.Provider value={{ appState, setAppState }}>
//       <Component {...pageProps} />
//     </AppStateContext.Provider>
//   );
// }

export {};
