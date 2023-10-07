import { AppStateContext } from '@/pages/_app';
import { Data, UTxO, getAddressDetails } from 'lucid-cardano';
import { useCallback, useContext, useEffect, useState } from 'react';
import { DataListingDatumType } from './DataListing';
import { TokenListing } from '@/services/token-listings/interface';
import { fetchTokenListingsApi } from '@/utilities/api';
import { extractPolicyIdFromAssetClass, getFinalScript } from './AcceptBid';
import { signAndSubmitTx } from '@/utilities/utilities';
import { truncateMiddle } from '@/utilities/text';

export const BidDatumSchema = Data.Object({
  dataBuyer: Data.Bytes(),
});

export type BidDatumType = Data.Static<typeof BidDatumSchema>;
export const BidDatum = BidDatumSchema as unknown as BidDatumType;

const BidRedeemerSchema = Data.Enum([Data.Literal('Redeem'), Data.Literal('Sell')]);
export type BidRedeemerType = Data.Static<typeof BidRedeemerSchema>;
export const BidRedeemer = BidRedeemerSchema as unknown as BidRedeemerType;

function BidSection() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex, dataListingScript } = appState;

  const [openBidAssetClass, setOpenBidAssetClass] = useState<string | null>(null);
  const [bidAmount, setBidAmount] = useState<bigint>(0n);
  const [tokens, setTokens] = useState<TokenListing[]>([]);

  // Assume bids is an array of objects that contain the user's active bids
  // For demonstration, each object has assetClass, date, and amount
  const [bids, setBids] = useState([
    { assetClass: 'abcd1234', date: '2022-12-01', amount: 20 },
    { assetClass: 'efgh5678', date: '2022-12-05', amount: 40 },
    // ...more bids
  ]);

  useEffect(() => {
    fetchTokenListings();
  }, []);

  const handleBidAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setBidAmount(BigInt(e.target.value));
  };

  const handleCopy = useCallback((textToCopy: string) => {
    navigator.clipboard.writeText(textToCopy).then(
      () => {
        // Clipboard successfully set
      },
      () => {
        // Clipboard write failed
      }
    );
  }, []);

  const handleBid = async (assetClass: string) => {
    console.log('handleBid', assetClass);
  };

  const fetchTokenListings = async () => {
    const tokenListings = await fetchTokenListingsApi();
    setTokens(tokenListings);
  };

  const submitBid = async (tokenAssetClass: string) => {
    if (!lucid || !wAddr) {
      console.error('Lucid not initialized');
      return;
    }
    console.log('Submitting bid for', tokenAssetClass, 'of amount', bidAmount);
    // Perform the actual bid submission here
    if (bidAmount <= 0n) {
      console.error('Bid amount must be greater than 0');
      return;
    }

    const [tokenPolicyId, tokenNameHex] = extractPolicyIdFromAssetClass(tokenAssetClass);

    const spendingValidator = getFinalScript(tokenPolicyId, tokenNameHex);
    const address = lucid.utils.validatorToAddress(spendingValidator);

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || '';

    const datum: BidDatumType = {
      dataBuyer: pkh,
    };

    const tx = await lucid!
      .newTx()
      .payToContract(address, { inline: Data.to(datum, BidDatum) }, { lovelace: bidAmount * 1_000_000n })
      .addSignerKey(pkh)
      .complete();
    const txId = await signAndSubmitTx(tx);
    console.log(`Bid placed tx: ${txId}`);

    // Close the bid input
    setOpenBidAssetClass(null);
  };

  // Function to redeem bid
  const redeemBid = (assetClass: string) => {
    console.log(`Redeeming bid for ${assetClass}`);
    // Implement your redeem logic here

    // Remove the redeemed bid from the bids state
    setBids(bids.filter((bid) => bid.assetClass !== assetClass));
  };

  return (
    <section className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[864px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mt-5 overflow-x-auto">
      <div className="flex flex-row justify-between items-center mb-5">
        <h2 className="text-2xl mb-4">Bid on Tokens</h2>
        <button
          onClick={fetchTokenListings}
          className="w-16 h-16 rounded-full bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
        >
          🔄
        </button>
      </div>

      <table className="w-full max-w-full text-left border-collapse">
        <thead>
          <tr>
            <th className="p-3 border-b-2 border-zinc-700">Asset Class</th>
            <th className="p-3 border-b-2 border-zinc-700">Owner Address</th>
            <th className="p-3 border-b-2 border-zinc-700">Metadata</th>
            <th className="p-3 border-b-2 border-zinc-700">Actions</th>
          </tr>
        </thead>
        <tbody>
          {tokens.map((token) => (
            <tr key={token.tokenAssetClass}>
              <td className="p-3 border-b border-zinc-700">
                <span>{truncateMiddle(token.tokenAssetClass)}</span>
                <button onClick={() => handleCopy(token.tokenAssetClass)} className="ml-2">
                  📋
                </button>
              </td>
              <td className="p-3 border-b border-zinc-700">
                <span>{truncateMiddle(token.owner)}</span>
                <button onClick={() => handleCopy(token.owner)} className="ml-2">
                  📋
                </button>
              </td>
              <td className="p-3 border-b border-zinc-700">{token?.meta}</td>
              <td className="p-3 border-b border-zinc-700">
                {openBidAssetClass === token.tokenAssetClass ? (
                  <div className="flex items-center space-x-2">
                    <input
                      type="number"
                      placeholder="Enter ADA"
                      className="p-2 rounded-lg border border-zinc-600"
                      onChange={handleBidAmountChange}
                    />
                    <button
                      onClick={() => submitBid(token.tokenAssetClass)}
                      className="rounded-lg p-2 text-zinc-50 bg-zinc-800"
                      disabled={bidAmount <= 0n}
                    >
                      Submit
                    </button>
                    <button onClick={() => setOpenBidAssetClass(null)} className="text-red-500">
                      ❌
                    </button>
                  </div>
                ) : (
                  <button
                    onClick={() => setOpenBidAssetClass(token.tokenAssetClass)}
                    className="rounded-lg p-2 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                  >
                    Place Bid
                  </button>
                )}
              </td>
            </tr>
          ))}
        </tbody>
      </table>

      {/* New section for viewing bids */}
      <section className="mt-10">
        <h3>Your Active Bids</h3>
        <table className="w-full max-w-full text-left border-collapse mt-4">
          <thead>
            <tr>
              <th className="p-3 border-b-2 border-zinc-700">Asset Class</th>
              <th className="p-3 border-b-2 border-zinc-700">Date</th>
              <th className="p-3 border-b-2 border-zinc-700">Amount</th>
              <th className="p-3 border-b-2 border-zinc-700">Actions</th>
            </tr>
          </thead>
          <tbody>
            {bids.map((bid, index) => (
              <tr key={index}>
                <td className="p-3 border-b border-zinc-700">{truncateMiddle(bid.assetClass)}</td>
                <td className="p-3 border-b border-zinc-700">{bid.date}</td>
                <td className="p-3 border-b border-zinc-700">{bid.amount} ADA</td>
                <td className="p-3 border-b border-zinc-700">
                  <button onClick={() => redeemBid(bid.assetClass)} className="rounded-lg p-2 text-zinc-50 bg-red-600">
                    Redeem
                  </button>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </section>
    </section>
  );
}

export default BidSection;
