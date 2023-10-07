import { AppStateContext } from '@/pages/_app';
import { Data, UTxO } from 'lucid-cardano';
import { useCallback, useContext, useEffect, useState } from 'react';
import { DataListingDatumType } from './DataListing';
import { TokenListing } from '@/services/token-listings/interface';
import { fetchTokenListingsApi } from '@/utilities/api';

export const BidDatumSchema = Data.Object({
  dataBuyer: Data.Bytes(),
});

export type BidDatumType = Data.Static<typeof BidDatumSchema>;
export const BidDatum = BidDatumSchema as unknown as BidDatumType;

const DataListingRedeemerSchema = Data.Enum([Data.Literal('Redeem'), Data.Literal('Purchase')]);
type DataListingRedeemer = Data.Static<typeof DataListingRedeemerSchema>;
const DataListingRedeemer = DataListingRedeemerSchema as unknown as DataListingRedeemer;

function BidSection() {
  const { appState, setAppState } = useContext(AppStateContext);
  const { lucid, wAddr, dataTokenPolicyIdHex, dataListingScript } = appState;

  const [openBidAssetClass, setOpenBidAssetClass] = useState<string | null>(null);
  const [bidAmount, setBidAmount] = useState<number>(0);
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

  // Function to truncate text in the middle
  const truncateMiddle = (text: string, startChars = 6, endChars = 6, separator = '...') => {
    if (text.length <= startChars + endChars) {
      return text;
    }
    return text.substr(0, startChars) + separator + text.substr(text.length - endChars);
  };

  const handleBidAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setBidAmount(Number(e.target.value));
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

  const submitBid = (assetClass: string) => {
    console.log('Submitting bid for', assetClass, 'of amount', bidAmount);
    // Perform the actual bid submission here

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
        <h2>Bid on Tokens</h2>
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
