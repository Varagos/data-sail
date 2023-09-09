import { AppStateContext } from '@/pages/_app';
import { Data, Lucid, Script, UTxO } from 'lucid-cardano';
import { useContext, useEffect, useState } from 'react';
import { DataListingDatum, DataListingDatumType } from './DataListing';

type UtxoEntry = {
  id: string;
  value: bigint;
};

type DataListingUTxOs = Array<{ utxo: UTxO; datum: DataListingDatumType }>;

function Buyer() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex, dataListingScript } = appState;
  const [utxos, setUtxos] = useState<UtxoEntry[]>([
    {
      id: '123',
      value: BigInt(100),
    },
    {
      id: '456',
      value: BigInt(200),
    },
  ]);
  const [selectedUtxo, setSelectedUtxo] = useState<string | null>(null);

  async function dataListingUTxOs(lucid: Lucid, dataListingScript: Script): Promise<DataListingUTxOs> {
    const dataListingAddress = lucid.utils.validatorToAddress(dataListingScript);
    const utxos = await lucid.utxosAt(dataListingAddress);
    const res: DataListingUTxOs = [];
    for (const utxo of utxos) {
      const datum = utxo.datum;
      if (datum) {
        try {
          const d = Data.from<DataListingDatumType>(datum, DataListingDatum);
          res.push({
            utxo: utxo,
            datum: d,
          });
        } catch (err) {
          // ignore different datum types
        }
      }
    }
    return res;
  }
  const fetchUtxos = async () => {
    if (!lucid) {
      console.error('Lucid not initialized');
      return;
    }
    const utxos = await dataListingUTxOs(lucid, dataListingScript);
    const utxoEntries: UtxoEntry[] = utxos.map((utxo) => ({
      id: `${utxo.utxo.txHash}#${utxo.utxo.outputIndex}`,
      value: utxo.datum.price,
    }));
    setUtxos(utxoEntries);
  };

  useEffect(() => {
    // fetch your UTXOs here and set them in state
    fetchUtxos();
  }, []);

  const handleBuy = () => {
    // handle purchase of selected UTXO here
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <div className="flex flex-row justify-between items-center mb-5">
          <h2>Available Data Listings</h2>
          <button
            onClick={fetchUtxos}
            className="w-16 h-16 rounded-full bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
          >
            🔄
          </button>
        </div>
        <table className="w-full text-left border-collapse">
          <thead>
            <tr>
              <th className="p-3 border-b-2 border-zinc-700">UTXO ID</th>
              <th className="p-3 border-b-2 border-zinc-700">Value</th>
            </tr>
          </thead>
          <tbody>
            {utxos.map((utxo) => (
              <tr
                key={utxo.id}
                onClick={() => setSelectedUtxo(utxo.id)}
                className={selectedUtxo === utxo.id ? 'bg-zinc-200' : ''}
              >
                <td className="p-3 border-b border-zinc-700">{utxo.id}</td>
                <td className="p-3 border-b border-zinc-700">{utxo.value.toString()} Lovelaces</td>
              </tr>
            ))}
          </tbody>
        </table>
        <div className="flex flex-col mt-5">
          <button
            onClick={handleBuy}
            className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            disabled={!selectedUtxo}
          >
            Buy Selected UTXO
          </button>
        </div>
      </div>
    </div>
  );
}

export default Buyer;
