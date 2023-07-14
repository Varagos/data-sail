import React, { useState } from 'react';
import { sentence } from 'txtgen';
import CryptoJS from 'crypto-js';
import ButtonDarkFullWidth from './elements/dark-full-w-button';
import { storage } from '@/utilities/storage';

function DataListing() {
  const [randomData, setRandomData] = useState('');
  const [encryptedData, setEncryptedData] = useState('');
  const [encryptionKey, setEncryptionKey] = useState('No Key');

  const generateData = () => {
    setRandomData(sentence());
  };
  const storeAndEncryptData = () => {
    /***
     * CryptoJS.lib.WordArray.random(128/8) generates a 128-bit random encryption key.
     * The .toString(CryptoJS.enc.Hex) converts the key to a hex string for easier handling.
     */
    const key = CryptoJS.lib.WordArray.random(128 / 8).toString(CryptoJS.enc.Hex);
    setEncryptionKey(key);
    const ciphertext = CryptoJS.AES.encrypt(randomData, key).toString();
    setEncryptedData(ciphertext);
    // Here you can store the ciphertext wherever you want
    console.log({ encryptedData: ciphertext });
    storage.storeData(ciphertext);
    decryptData(ciphertext, key);
  };

  const hideKey = () => {
    setEncryptionKey('No Key');
  };

  const decryptData = (encryptedData: string, encryptionKey: string) => {
    const bytes = CryptoJS.AES.decrypt(encryptedData, encryptionKey);
    const originalData = bytes.toString(CryptoJS.enc.Utf8);

    console.log(originalData); // log the decrypted data to console, or use it however you wish
    return originalData;
  };

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5">
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={generateData}>Generate Random Data</ButtonDarkFullWidth>
          <p className="my-2">{randomData}</p>
        </div>
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={storeAndEncryptData}>Store encrypted Data & Mint NFT</ButtonDarkFullWidth>
          <div className="flex justify-between items-center mt-2">
            <p className="my-2 flex-grow">{encryptionKey}</p>
            <button
              onClick={hideKey}
              className="w-16 rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              Hide Key
            </button>
          </div>
        </div>
        <div className="flex flex-col mb-2">
          <ButtonDarkFullWidth clickHandler={() => {}}>List Data for Sale</ButtonDarkFullWidth>
        </div>
      </div>
    </div>
  );
}

export default DataListing;
