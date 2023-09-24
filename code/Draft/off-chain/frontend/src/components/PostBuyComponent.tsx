import { useContext, useEffect } from 'react';
import { BuyStatus } from './Buyer';
import { AppStateContext } from '@/pages/_app';
import { retrieveHistoryForBuyer } from '@/utilities/api';

function PostBuyComponent({
  buyStatus,
  tokenAssetClass,
  setBuyStatus,
}: {
  buyStatus: BuyStatus;
  tokenAssetClass: string;
  setBuyStatus: (status: BuyStatus) => void;
}) {
  const { appState } = useContext(AppStateContext);
  const { lucid, wAddr } = appState;

  useEffect(() => {
    let timer: NodeJS.Timeout;
    if (buyStatus === BuyStatus.Completed) {
      // Poll for the token entering the wallet
      timer = setInterval(async () => {
        if (!lucid) {
          return;
        }
        const utxos = await lucid.wallet.getUtxos();
        if (utxos.find((u) => u.assets[tokenAssetClass] !== undefined)) {
          console.log('Found token!');
          clearInterval(timer);
          // Update state to indicate that data is ready to be downloaded
          setBuyStatus(BuyStatus.DataReady);
        }
      }, 2000); // Poll every 2 seconds, for example
    }
    // On component unmount, clear the timer
    return () => {
      if (timer) {
        clearInterval(timer);
      }
    };
  }, [buyStatus, tokenAssetClass]);

  const downloadData = async () => {
    if (!wAddr) {
      throw new Error('No wallet address');
    }
    const result = await retrieveHistoryForBuyer(tokenAssetClass, wAddr);
    saveTemplateAsFile('data.json', result);
  };

  const saveTemplateAsFile = (filename: string, dataObjToWrite: Record<string, any>) => {
    const blob = new Blob([JSON.stringify(dataObjToWrite)], { type: 'text/json' });
    const link = document.createElement('a');

    link.download = filename;
    link.href = window.URL.createObjectURL(blob);
    link.dataset.downloadurl = ['text/json', link.download, link.href].join(':');

    const evt = new MouseEvent('click', {
      view: window,
      bubbles: true,
      cancelable: true,
    });

    link.dispatchEvent(evt);
    link.remove();
  };

  return (
    <div>
      {buyStatus === BuyStatus.Waiting && <p>Waiting for transaction to complete...</p>}
      {buyStatus === BuyStatus.Completed && <p>Transaction completed. Checking for token...</p>}
      {buyStatus === BuyStatus.DataReady && <button onClick={downloadData}>Download Data</button>}
    </div>
  );
}

export default PostBuyComponent;
