import React, { useEffect } from 'react';
import ReactDOM from 'react-dom';
import { Lucid, Blockfrost } from 'lucid-cardano';

function Sandbox() {
  useEffect(() => {
    const connectLucidAndNami = async () => {
      const blackFrostProjectId = 'previewsKVSTHsY3c2sWpsRpJRjs9KiOLaSRmht'; // process.env.REACT_APP_BLACKFROST_KEY;
      console.log({ blackFrostProjectId });
      const lucid = await Lucid.new(
        new Blockfrost('https://cardano-preview.blockfrost.io/api/v0', blackFrostProjectId),
        'Preview'
      );
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
    // Listener for messages from the popup
    // window.addEventListener('message', (event) => {
    //   // event.source?.postMessage({ type: 'response', message: 'Hello Popup!' }, event.origin as any);
    //   const result = event.data;
    //   event.source!.postMessage({ result: result }, event.origin as any);
    // });
    connectLucidAndNami();
  }, []);

  console.log('FINALLY??');
  return (
    <div>
      <p>Sandbox content</p>
    </div>
  );
}

const root = document.createElement('div');
document.body.appendChild(root);

ReactDOM.render(<Sandbox />, root);
