import { useEffect, useState } from 'react';

type UtxoEntry = {
  id: string;
  value: number;
};

function Buyer() {
  const [utxos, setUtxos] = useState<UtxoEntry[]>([
    {
      id: '123',
      value: 100,
    },
    {
      id: '456',
      value: 200,
    },
  ]);
  const [selectedUtxo, setSelectedUtxo] = useState<string | null>(null);

  useEffect(() => {
    // fetch your UTXOs here and set them in state
  }, []);

  const refreshUtxos = () => {
    // re-fetch UTXOs here
  };

  const handleBuy = () => {
    // handle purchase of selected UTXO here
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <div className="flex flex-row justify-between items-center mb-5">
          <h2>Available Data Listings</h2>
          <button
            onClick={refreshUtxos}
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
                <td className="p-3 border-b border-zinc-700">{utxo.value}</td>
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
