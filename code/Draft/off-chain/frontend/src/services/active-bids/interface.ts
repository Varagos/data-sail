export type ActiveBid = {
  id: string; // txHash#outputIndex
  address: string;
  tokenAssetClass: string;
  amount: number;
  date: string;
};
export interface IActiveBids {
  addActiveBid(wallet: string, activeBid: ActiveBid): Promise<void>;
  fetchAllActiveBids(wallet: string): Promise<ActiveBid[]>;
  removeActiveBid(wallet: string, bidId: string): Promise<void>;
}
