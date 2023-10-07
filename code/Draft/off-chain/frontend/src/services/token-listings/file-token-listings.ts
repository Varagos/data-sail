import { resolve } from 'path';
import { BaseFileKeyValueStorage } from '../common/file-storage';
import { ITokenListings, TokenListing } from './interface';

export default class FileTokenListings extends BaseFileKeyValueStorage<string> implements ITokenListings {
  constructor(fileName: string = 'token-listings.json') {
    // /workspace/code/Draft/off-chain/frontend/data/data.json
    const projectRoot = resolve(__dirname, '..', '..', '..', '..');
    console.log({ projectRoot });
    const dataPath = resolve(projectRoot, 'data');
    const filePath = resolve(dataPath, fileName);
    super(filePath);
  }

  async addTokenListing(tokenListing: TokenListing): Promise<void> {
    const value = JSON.stringify(tokenListing);
    await this.addEntry(tokenListing.tokenAssetClass, value);
  }
  async fetchAllDataListings(): Promise<TokenListing[]> {
    const storage = await this.readFromFile();
    const tokenListings = Array.from(storage.values()).map((value) => JSON.parse(value));
    return tokenListings;
  }

  async removeTokenListing(tokenAssetClass: string): Promise<void> {
    return this.removeEntry(tokenAssetClass);
  }
}
