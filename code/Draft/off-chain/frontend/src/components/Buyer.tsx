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

  async function fetchTxInputAddresses(txHash: string) {
    const response = await fetch(`/api/getTxInfo?txHash=${txHash}`);
    const data = await response.json();
    return data;
  }

  /**
   * Find using txHash and some provider
   */
  const findSellerAddress = async (lockedUtxo: UTxO, pubKeyHash: string): Promise<string> => {
    const { txHash } = lockedUtxo;
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
    console.log('handleBuy');
    if (!selectedUtxo) {
      console.error('No UTXO selected');
      return;
    }
    const selectedUtxoDatum = selectedUtxo.datum;

    const sellerAddress = await findSellerAddress(selectedUtxo.utxo, selectedUtxoDatum.dataSeller);
    console.log({ sellerAddress });
    const tokenAssetClass = getUtxoLockedTokenAssetClass(selectedUtxo.utxo);
    // Add a download button using the tokenAssetClass (fetch the data, and have them ready for download)
    // make the button available when token gets detected in user's wallet

    const metadata = await lucid.metadataOf(tokenAssetClass);

    const redeemer = Data.to<DataListingRedeemer>('Purchase', DataListingRedeemer);
    setBuyStatus(BuyStatus.Waiting);
    setTokenAssetClass(tokenAssetClass);
    try {
      const tx = await lucid
        .newTx()
        .payToAddress(sellerAddress, {
          lovelace: selectedUtxoDatum.price,
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
          <h2 className="text-2xl mb-4">Ask - Available Data Listings</h2>
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
