import { FiShoppingCart } from 'react-icons/fi';
import { AppStateContext } from '@/pages/_app';
import { Data, Lucid, Script, UTxO, getAddressDetails, toText } from 'lucid-cardano';
import { useContext, useEffect, useState } from 'react';
import { BlockFrostAPI } from '@blockfrost/blockfrost-js';
import { DataListingDatum, DataListingDatumType } from './DataListing';
import { signAndSubmitTx } from '@/utilities/utilities';
import PostBuyComponent from './PostBuyComponent';
import BidSection from './BidSection';
import PostBidAccepted from './PostBidAccepted';

type UtxoEntry = {
  id: string;
  value: bigint;
  utxo: UTxO;
  datum: DataListingDatumType;
};

type DataListingUTxOs = Array<{ utxo: UTxO; datum: DataListingDatumType }>;

const DataListingRedeemerSchema = Data.Enum([Data.Literal('Redeem'), Data.Literal('Purchase')]);
type DataListingRedeemer = Data.Static<typeof DataListingRedeemerSchema>;
const DataListingRedeemer = DataListingRedeemerSchema as unknown as DataListingRedeemer;

export enum BuyStatus {
  Initiating,
  Waiting,
  Completed,
  DataReady,
}

function Buyer() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex, dataListingScript } = appState;
  const [utxos, setUtxos] = useState<UtxoEntry[]>([]);
  const [selectedUtxo, setSelectedUtxo] = useState<UtxoEntry | null>(null);

  const [buyStatus, setBuyStatus] = useState<BuyStatus>(BuyStatus.Initiating);
  const [tokenAssetClass, setTokenAssetClass] = useState<string>('');

  async function dataListingUTxOs(lucid: Lucid, dataListingScript: Script): Promise<DataListingUTxOs> {
    const dataListingAddress = lucid.utils.validatorToAddress(dataListingScript);
    const res: DataListingUTxOs = [];
    try {
      const utxos = await lucid.utxosAt(dataListingAddress);
      for (const utxo of utxos) {
        const datum = utxo.datum;
        if (datum) {
          try {
            const d = Data.from<DataListingDatumType>(datum, DataListingDatum);
            res.push({
              utxo,
              datum: d,
            });
          } catch (err) {
            // ignore different datum types
          }
        }
      }
      return res;
    } catch (err) {
      // Throws if no UTXOs found
      console.error(err);
      return res;
    }
  }
  const fetchUtxos = async () => {
    if (!lucid) {
      console.error('Lucid not initialized');
      return;
    }
    const utxos = await dataListingUTxOs(lucid, dataListingScript);
    const utxoEntries: UtxoEntry[] = utxos.map(({ utxo, datum }) => ({
      id: `${utxo.txHash}#${utxo.outputIndex}`,
      value: datum.price,
      utxo,
      datum,
    }));
    setUtxos(utxoEntries);
  };

  useEffect(() => {
    // fetch your UTXOs here and set them in state
    fetchUtxos();
  }, []);

  const getSellerAddress = (utxoDatum: DataListingDatumType): string => {
    if (!lucid) {
      throw new Error('Lucid not initialized');
    }
    // return 'addr_test1qqdh2vsgs4pnwwtdrrupp3ltzhk32z96hwkv43hy5n3335jrvsmd2enzlguaf7hqcyt5urk9y5hvjs3erv50da6l2zzq0etu68';
    const sellerPubKeyHash = utxoDatum.dataSeller;

    // const sellerAddress = toText(sellerAddressHex);
    // // We are misssing the staking credentials to create the address just from the pubkeyhash
    // // Nami and others wallets automatically add the staking credential when creating new wallets
    // console.log({ sellerAddressHex, sellerAddress });
    // console.log({ utxoSellerKeyHash: sellerPubKeyHash });
    const paymentCredential = lucid.utils.keyHashToCredential(sellerPubKeyHash);
    console.log({ credentials: paymentCredential.type });
    //TODO Look for utxo using blokfrost, and find address that includes this pubkeyhash
    const address = lucid.utils.credentialToAddress(paymentCredential);
    return address;
  };

  // {
  //     "sellerAddressHex": "616464725f746573743171716468327673677334706e777774647272757070336c747a686b33327a393668776b7634336879356e333333356a7276736d6432656e7a6c677561663768716379743575726b39793568766a733365727635306461366c327a7a71306574753638",
  //     "sellerAddress": "addr_test1qqdh2vsgs4pnwwtdrrupp3ltzhk32z96hwkv43hy5n3335jrvsmd2enzlguaf7hqcyt5urk9y5hvjs3erv50da6l2zzq0etu68"
  // }

  /**
   * Testing User1 (Seller){
    "waddr": "addr_test1qqdh2vsgs4pnwwtdrrupp3ltzhk32z96hwkv43hy5n3335jrvsmd2enzlguaf7hqcyt5urk9y5hvjs3erv50da6l2zzq0etu68",
    "pkh": "1b753208854337396d18f810c7eb15ed1508babbaccac6e4a4e318d2"
}


Datum has pubkeyhash: {
    "utxoSellerKeyHash": "1b753208854337396d18f810c7eb15ed1508babbaccac6e4a4e318d2"
}
correct, but getSellerAddress returns address
{
    "sellerAddress": "addr_test1vqdh2vsgs4pnwwtdrrupp3ltzhk32z96hwkv43hy5n3335spv85en"
}


WHEN ADDDING STAKE CREDENTIAL:
{
    "sellerAddress": "addr_test1qqdh2vsgs4pnwwtdrrupp3ltzhk32z96hwkv43hy5n3335jrvsmd2enzlguaf7hqcyt5urk9y5hvjs3erv50da6l2zzq0etu68"
}
   */
  async function fetchTxInputAddresses(txHash: string) {
    try {
      const response = await fetch(`/api/getTxInfo?txHash=${txHash}`);
      const data = await response.json();
      return data;
    } catch (error) {
      console.error('An error occurred while fetching the transaction:', error);
      return null;
    }
  }

  /**
   * Find using txHash and some provider
   */
  const findSellerAddress = async (lockedUtxo: UTxO, pubKeyHash: string): Promise<string> => {
    const { txHash } = lockedUtxo;
    console.log({
      txHash,
    });
    // const tx = `${txHash}#${outputIndex}`;
    // Tx that locked the utxo, we are trying to find the address that locked it
    // const utxos = await API.txsUtxos(txHash);
    const txInfo = await fetchTxInputAddresses(txHash);
    const inputAddresses: string[] = txInfo.inputAddress;
    const addressesMatchingKeyHash = inputAddresses.filter(
      (addr) => getAddressDetails(addr).paymentCredential?.hash === pubKeyHash
    );
    if (addressesMatchingKeyHash.length === 0) {
      // Fallback to address directly from pubkeyhash
      throw new Error('No addresses found matching the key hash');
    }
    // Multiple addresses because of different staking credentials
    // Return the largest
    return addressesMatchingKeyHash.sort((a, b) => b.length - a.length)[0];

    // return txHash;
  };

  const getUtxoLockedTokenAssetClass = (utxo: UTxO): string => {
    const assets = utxo.assets;
    let assetClasses = Object.keys(assets);
    // filter out locked ada token
    assetClasses = assetClasses.filter((assetClass) => assetClass !== 'lovelace');
    if (assetClasses.length !== 1) {
      throw new Error('UTXO has more than one asset');
    }
    return assetClasses[0];
  };

  const handleBuy = async () => {
    setBuyStatus(BuyStatus.Initiating);
    if (!lucid) {
      console.error('Lucid not initialized');
      return;
    }
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || '';
    // console.log({ waddr: wAddr!, pkh });
    console.log('handleBuy');
    if (!selectedUtxo) {
      console.error('No UTXO selected');
      return;
    }
    const selectedUtxoDatum = selectedUtxo.datum;
    // const sellerAddress = getSellerAddress(selectedUtxoDatum);
    // const dataListingAddress = lucid.utils.validatorToAddress(dataListingScript);
    // console.log({ sellerAddress });

    const sellerAddress = await findSellerAddress(selectedUtxo.utxo, selectedUtxoDatum.dataSeller);
    console.log({ sellerAddress });
    const tokenAssetClass = getUtxoLockedTokenAssetClass(selectedUtxo.utxo);
    // Add a download button using the tokenAssetClass (fetch the data, and have them ready for download)
    // make the button available when token gets detected in user's wallet
    console.log({ tokenAssetClass });

    // console.log({ selectedUtxo: selectedUtxo.utxo });
    // handle purchase of selected UTXO here
    const redeemer = Data.to<DataListingRedeemer>('Purchase', DataListingRedeemer);
    console.log({ redeemer });
    setBuyStatus(BuyStatus.Waiting);
    setTokenAssetClass(tokenAssetClass);
    try {
      const tx = await lucid
        .newTx()
        .payToAddress(sellerAddress, {
          lovelace: selectedUtxoDatum.price,
          // lovelace: 10_000_000n, //selectedUtxoDatum.price,
        })
        .collectFrom([selectedUtxo.utxo], redeemer)
        .attachSpendingValidator(dataListingScript)
        // .addSignerKey(pkh)
        .complete({ nativeUplc: false });

      const txId = await signAndSubmitTx(tx);
      console.log('txId', txId);
      setBuyStatus(BuyStatus.Completed);
    } catch (err) {
      setBuyStatus(BuyStatus.Initiating);
      console.error(err);
    }
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[864px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5 overflow-x-auto">
        <div className="flex flex-row justify-between items-center mb-5">
          <h2 className="text-2xl mb-4">Available Data Listings</h2>
          <button
            onClick={fetchUtxos}
            className="w-16 h-16 rounded-full bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
          >
            🔄
          </button>
        </div>
        <table className="w-full max-w-full text-left border-collapse">
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
                onClick={() => setSelectedUtxo(utxo)}
                className={selectedUtxo?.id === utxo.id ? 'bg-zinc-200' : ''}
              >
                <td className="p-3 border-b border-zinc-700 whitespace-nowrap overflow-hidden">{utxo.id}</td>
                <td className="p-3 border-b border-zinc-700 whitespace-nowrap overflow-hidden">
                  {utxo.value.toString()} Lovelaces
                </td>
              </tr>
            ))}
          </tbody>
        </table>
        <div className="flex flex-col mt-5 space-y-4">
          <button
            onClick={handleBuy}
            className="flex items-center justify-center w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            disabled={!selectedUtxo}
          >
            <FiShoppingCart className="mr-2" />
            Buy Selected UTXO
          </button>
          <PostBuyComponent buyStatus={buyStatus} tokenAssetClass={tokenAssetClass} setBuyStatus={setBuyStatus} />
        </div>
      </div>
      <BidSection />
      <PostBidAccepted />
    </div>
  );
}

export default Buyer;
