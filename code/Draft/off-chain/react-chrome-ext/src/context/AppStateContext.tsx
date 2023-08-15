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
import React, { createContext, useEffect, useState } from 'react';

export type AppState = {
  lucid?: Lucid;
  wAddr?: Address;
  // Your AppState type here
  dataTokenPolicyIdHex?: any;
};

const initialAppState: AppState = {};

export const AppStateContext: React.Context<{
  appState: AppState;
  setAppState: React.Dispatch<React.SetStateAction<AppState>>;
}> = createContext<{
  appState: AppState;
  setAppState: React.Dispatch<React.SetStateAction<AppState>>;
}>({ appState: initialAppState, setAppState: () => {} });

export default function AppStateProvider({ children }: any) {
  const [appState, setAppState] = useState<AppState>(initialAppState);

  const connectLucidAndNami = async () => {
    const blackFrostProjectId = 'previewsKVSTHsY3c2sWpsRpJRjs9KiOLaSRmht'; // process.env.REACT_APP_BLACKFROST_KEY;
    console.log({ blackFrostProjectId });
    // const lucid = await Lucid.new(
    //   new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blackFrostProjectId),
    //   'Preview'
    // );
    console.log('before nami');
    // if (!window.cardano.nami) {
    //   window.alert('Please install Nami Wallet');
    //   return;
    // }
    // const nami = await window.cardano.nami.enable();
    // console.log('after nami enabled');
    // lucid.selectWallet(nami);
    // console.log('after lucid select wallet');
    // setAppState({
    //   ...initialAppState,
    //   lucid: lucid,
    //   wAddr: await lucid.wallet.address(),
    // });
  };

  useEffect(() => {
    if (appState.lucid) return;
    connectLucidAndNami();
  }, [appState]);

  useEffect(() => {
    // Reference to the sandbox iframe
    const iframe = document.getElementById('mySandbox') as HTMLIFrameElement;
    console.log('picked up iframe', iframe);

    // Listener for messages from the sandbox
    window.addEventListener('message', (event) => {
      console.log('Received message from sandbox:', event.data);
    });

    // Function to handle iframe load
    const handleIframeLoad = () => {
      console.log('Iframe loaded. Sending message...');

      // Send a message to the sandbox
      iframe.contentWindow?.postMessage({ type: 'greeting', message: 'Hello Sandbox!' }, '*');
    };

    // Attach the load event to the iframe
    iframe.addEventListener('load', handleIframeLoad);
    console.log('added event listener');

    // TODO Check the iframe content to check if it is the sandbox component we want
    // or if iframe's src is wrong

    return () => {
      // Cleanup - remove the event listeners
      iframe.removeEventListener('load', handleIframeLoad);
    };
  }, []);

  return <AppStateContext.Provider value={{ appState, setAppState }}>{children}</AppStateContext.Provider>;
}
