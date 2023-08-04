import { useState } from 'react';
import './App.css';
import Buyer from './components/Buyer';

// import { AppStateContext } from './context/AppStateContext';

function App() {
  type Person = 'seller' | 'buyer';
  const [isPerson, setIsPerson] = useState<Person>('seller');

  const handleClick = (v: Person) => {
    if (v === 'seller') {
      setIsPerson('seller');
    } else if (v === 'buyer') {
      setIsPerson('buyer');
    }
    console.log(v);
  };

  return (
    <main className="w-96 h-auto min-h-96 rounded-lg bg-zinc-800 shadow-lg p-6 flex flex-col items-center gap-8">
      {/* PERSON BUTTONS - ABSOLUTE POSITION */}
      <div className="flex flex-row gap-4 rounded-b-lg bg-zinc-800">
        <button
          onClick={() => handleClick('seller')}
          className={`${
            isPerson == 'seller'
              ? 'bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]'
              : 'bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]'
          }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
        >
          Seller
        </button>

        <button
          onClick={() => handleClick('buyer')}
          className={`${
            isPerson == 'buyer'
              ? 'bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]'
              : 'bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]'
          }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
        >
          Buyer
        </button>
      </div>

      <div className="flex flex-col items-center gap-8  mt-5  h-full py-10 bg-zinc-50 w-4/5 rounded-2xl">
        {/* {
          isPerson === 'seller' && <DataListing />
          // <DataConsentComponent />
        } */}
        {/* {isPerson === 'buyer' && <Buyer />} */}
      </div>
    </main>
  );
}

export default App;
