import FileTokenListings from './file-token-listings';
import { ITokenListings } from './interface';

export const tokenListingStorageService: ITokenListings = new FileTokenListings();
