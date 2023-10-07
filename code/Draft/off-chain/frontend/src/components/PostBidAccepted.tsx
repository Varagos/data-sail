import { AppStateContext, TOKEN_NAME } from '@/pages/_app';
import type { DataSession } from '@/types';
import { retrieveHistoryForBuyer } from '@/utilities/api';
import { Lucid, fromText } from 'lucid-cardano';
import React, { useState, useEffect, useContext } from 'react';

type TokenValidity =
  | {
      isValid: false;
    }
  | {
      isValid: true;
      data: DataSession;
    };

const PostBidAccepted = () => {
  const { appState } = useContext(AppStateContext);
  // This is the dataTokenPolicyIdHex of the token the seller has minted now(if they have minted one)
  const { lucid, wAddr, dataTokenPolicyIdHex } = appState;

  const [walletTokens, setWalletTokens] = useState<string[]>([]);
  const [selectedToken, setSelectedToken] = useState<string | null>(null);
  const [isTokenValid, setIsTokenValid] = useState<TokenValidity>({ isValid: false });

  async function fetchWalletTokens(lucid: Lucid): Promise<string[]> {
    const tokenNameHex = fromText(TOKEN_NAME);
    const walletUtxos = await lucid.wallet.getUtxos();
    const allAssets = walletUtxos.map((utxo) => utxo.assets);
    const dataTokens: string[] = [];
    for (const assets of allAssets) {
      for (const [assetClass, value] of Object.entries(assets)) {
        if (assetClass === 'lovelace') {
          continue;
        }
        // Only display tokens that have a quantity of 1
        if (value !== 1n) {
          continue;
        }
        console.log('Found asset:');
        console.log([assetClass, value]);
        console.log(assetClass.endsWith(tokenNameHex));
        if (assetClass.endsWith(tokenNameHex)) {
          dataTokens.push(assetClass);
        }
      }
    }
    return dataTokens;
  }

  const scanWallet = async () => {
    // Implement wallet scanning logic
    if (!lucid) {
      console.error('Lucid not initialized');
      return;
    }
    const tokens = await fetchWalletTokens(lucid);
    setWalletTokens(tokens);
  };

  const validateToken = async () => {
    // Implement token validation logic
    if (!wAddr) {
      throw new Error('No wallet address');
    }
    if (!lucid) {
      throw new Error('No lucid');
    }
    if (!selectedToken) {
      throw new Error('No selected token');
    }

    const result = await retrieveHistoryForBuyer(selectedToken, wAddr, lucid);
    console.log({ retrivedHistoryForBuyer: result });
    setIsTokenValid({
      isValid: true,
      data: result,
    });
  };

  //   const downloadData = () => {
  //     // Implement data download logic
  //   };

  const downloadData = async (data: DataSession) => {
    saveTemplateAsFile(`data-${Date.now()}.json`, data);
  };

  const saveTemplateAsFile = (filename: string, dataObjToWrite: Record<string, any>) => {
    const blob = new Blob([JSON.stringify(dataObjToWrite, null, 2)], { type: 'text/json' });
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

  useEffect(() => {
    // Reset the token validity when changing the selected token
    setIsTokenValid({ isValid: false });
  }, [selectedToken]);

  return (
    <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[864px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mt-5 overflow-x-auto">
      <div className="flex flex-row justify-between items-center mb-5">
        <h2 className="text-2xl mb-4">Your Wallet</h2>
        <button
          onClick={scanWallet}
          className="px-5 py-3 rounded bg-blue-500 text-white shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
        >
          Scan
        </button>
      </div>

      <div className="flex flex-col items-start space-y-4">
        <select onChange={(e) => setSelectedToken(e.target.value)} className="p-3 rounded-lg border border-zinc-600">
          <option value="">Select Token</option>
          {walletTokens.map((token, index) => (
            <option key={index} value={token}>
              {token}
            </option>
          ))}
        </select>

        <div className="flex space-x-4">
          {selectedToken && (
            <button
              onClick={validateToken}
              className="rounded-lg p-2 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              Validate Token
            </button>
          )}

          {isTokenValid.isValid && (
            <button
              onClick={() => downloadData(isTokenValid.data)}
              className="rounded-lg p-2 text-zinc-50 bg-green-500 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              Download Data
            </button>
          )}
        </div>
      </div>
    </div>
  );
};

export default PostBidAccepted;
