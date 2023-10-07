import { useContext } from 'react';
import { AppStateContext } from './_app';
import { ExplorerLink, ExplorerLinkPrime } from '@/components/ExplorerLinks';
import { HiUserCircle } from 'react-icons/hi';
import { IoReloadCircleSharp } from 'react-icons/io5';
import { useState } from 'react';
import { Data, Lucid, fromHex } from 'lucid-cardano';
import DataListing from '@/components/DataListing';
import Buyer from '@/components/Buyer';

export default function Home() {
  type Person = 'oracle' | 'user' | 'owner' | 'seller' | 'buyer';
  const [isPerson, setIsPerson] = useState<Person>('seller');
  const { appState, setAppState } = useContext(AppStateContext);
  const {
    lucid,
    wAddr,
    // scPolicyIdHex,
    // scAssetClassHex,
    // oracleWithNftUTxO,
    // oracleAddress,
    // minPercent,
    // txScriptsDeployment,
    dataTokenPolicy,
    dataTokenPolicyIdHex,
    dataListingScript,
  } = appState;

  const refreshWallet = async () => {
    if (!appState.lucid || !window.cardano.nami) return;
    const nami = await window.cardano.nami.enable();
    appState.lucid.selectWallet(nami);
    setAppState({
      ...appState,
      wAddr: await appState.lucid.wallet.address(),
    });
  };

  const handleClick = (v: Person) => {
    if (v === 'oracle') {
      setIsPerson('oracle');
    } else if (v === 'user') {
      setIsPerson('user');
    } else if (v === 'owner') {
      setIsPerson('owner');
    } else if (v === 'seller') {
      setIsPerson('seller');
    } else if (v === 'buyer') {
      setIsPerson('buyer');
    }

    console.log(isPerson);
  };

  return (
    <main className="flex min-h-screen w-screen min-h-screen gap-6 flex-row-reverse items-center justify-between px-5 pb-5 pt-20 bg-zinc-800">
      <div className="flex flex-col items-center justify-start  w-[380px] mt-2">
        {/* USER LOGGED - ABSOLUTE POSITION */}
        <div className="absolute justify-center items-center right-0 top-5 bg-zinc-50  h-12  w-48 rounded-l-2xl flex flex-row">
          <HiUserCircle className="text-4xl text-zinc-600" onClick={refreshWallet} />
          <p className="text-lg mx-2 text-zinc-800">{wAddr ? `...${wAddr.substring(102)}` : ''}</p>
          <IoReloadCircleSharp className="text-3xl mx-2 text-zinc-600 active:text-zinc-800" onClick={refreshWallet} />
        </div>

        {/* INFORMATION TABLE */}
        <p className=" overflow-clip self-start tracking-[0.2em]  text-xs text-zinc-200">INFO TABLE</p>

        <div className=" overflow-hidden bg-zinc-50 rounded-lg w-full my-4 h-auto border border-spacing-1 border-zinc-50">
          <ExplorerLink
            message="Wallet: "
            type="address"
            value={wAddr ? `${wAddr.substring(0, 15)}...${wAddr.substring(100)}` : ''}
          />
          <ExplorerLink message="Data Token Policy Id in hex" type="policy" value={dataTokenPolicyIdHex || ''} />

          {/* Added by me */}
          <ExplorerLink
            message="Data Listing Address: "
            type="address"
            value={lucid?.utils.validatorToAddress(dataListingScript) || ''}
          />
        </div>
      </div>

      {/* PERSON BUTTONS - ABSOLUTE POSITION */}
      <div className="absolute top-4 left-5 flex flex-row gap-4">
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

      {/* ACTIONS SECTION - MAIN CONTAINER(WHITE AND ROUNDED) */}
      <div className="flex flex-col items-center gap-8  min-h-screen py-10 bg-zinc-50 w-4/5 rounded-2xl">
        {
          isPerson === 'seller' && <DataListing />
          // <DataConsentComponent />
        }
        {isPerson === 'buyer' && <Buyer />}
      </div>
    </main>
  );
}
