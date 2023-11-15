export type TokenListing = {
  owner: string;
  tokenAssetClass: string;
  // currencySymbol: string,
  // tokenName: string,
  meta?: any;
};
export interface ITokenListings {
  addTokenListing(tokenListing: TokenListing): Promise<void>;
  fetchAllDataListings(): Promise<TokenListing[]>;
  removeTokenListing(tokenAssetClass: string): Promise<void>;
}
