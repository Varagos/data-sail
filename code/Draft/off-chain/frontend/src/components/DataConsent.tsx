import { AppStateContext } from '@/pages/_app';
import { UTxO } from 'lucid-cardano';
import React, { useContext, useState } from 'react';

function DataConsentComponent() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, nftPolicyIdHex } = appState;

  const [beneficiary, setBeneficiary] = useState('');
  const [timePeriod, setTimePeriod] = useState('');
  const [browsingHistory, setBrowsingHistory] = useState(false);
  const [locationData, setLocationData] = useState(false);
  const [personalDetails, setPersonalDetails] = useState(false);
  const [utxo, setUtxo] = useState('');
  const [lovelaceAmount, setLovelaceAmount] = useState('');

  const getUtxo = async (address: string): Promise<UTxO> => {
    const utxos = await lucid!.utxosAt(address);
    const utxo = utxos[0];
    return utxo;
  };

  const handleSubmit = async () => {
    // Perform your action here
    console.log('Submitting');
    if (wAddr) {
      // check if we have an address, meaning a connected wallet
      const utxo = await getUtxo(wAddr);
      console.log(utxo);
    }
  };

  console.log('minting NFT for ' + wAddr);

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <div className="flex flex-col mb-2">
          <p>Beneficiary:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="string"
            value={beneficiary}
            onChange={(e) => setBeneficiary(e.target.value)}
          />
        </div>
        <div className="flex flex-col mb-2">
          <p>Time Period:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="string"
            value={timePeriod}
            onChange={(e) => setTimePeriod(e.target.value)}
          />
        </div>
        <div className="flex flex-col mb-2">
          <p>Browsing History Consent:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="checkbox"
            checked={browsingHistory}
            onChange={(e) => setBrowsingHistory(e.target.checked)}
          />
        </div>
        <div className="flex flex-col mb-2">
          <p>Location Data Consent:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="checkbox"
            checked={locationData}
            onChange={(e) => setLocationData(e.target.checked)}
          />
        </div>
        <div className="flex flex-col mb-2">
          <p>Personal Details Consent:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="checkbox"
            checked={personalDetails}
            onChange={(e) => setPersonalDetails(e.target.checked)}
          />
        </div>
        <div className="flex flex-col mb-2">
          <p>UTxO:</p>
          <input
            className="py-1 px-2 border border-zinc-700 rounded"
            type="string"
            value={utxo}
            onChange={(e) => setUtxo(e.target.value)}
          />
        </div>
        <div className="flex flex-row mb-2 items-baseline">
          <p>Lovelace Amount:</p>
          <input
            className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
            type="number"
            value={Number(lovelaceAmount)}
            onChange={(e) => setLovelaceAmount(e.target.value)}
          />
        </div>
        <div className="w-full flex flex-row justify-center mt-2">
          <button
            onClick={handleSubmit}
            className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
          >
            Send Consent
          </button>
        </div>
      </div>
    </div>
  );
}

export default DataConsentComponent;
