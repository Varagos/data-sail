import React, { useEffect, useState } from 'react';
import { postData } from './postData';

const Popup = () => {
  type Person = 'seller' | 'buyer';
  const [isPerson, setIsPerson] = useState<Person>('seller');
  const [visitedUrls, setVisitedUrls] = useState<
    Array<{ url: string; count: number }>
  >([]);
  // How many days of history to collect
  const [numberOfDays, setNumberOfDays] = useState<number>(7);
  const [walletAddress, setWalletAddress] = useState('');

  const handleClick = (v: Person) => {
    if (v === 'seller') {
      setIsPerson('seller');
    } else if (v === 'buyer') {
      setIsPerson('buyer');
    }
    console.log(v);
  };

  useEffect(() => {
    console.log('Popup mounted');

    chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
      console.log('popup received msg:');
      if (message.hasNamiWallet) {
        // Nami Wallet is installed, proceed as needed
      } else {
        // Nami Wallet is not installed, alert the user
        window.alert('Please install Nami Wallet');
      }
    });
  }, []);

  // Search history to find up to ten links that a user has typed in,
  // and show those links in a popup.
  function buildTypedUrlList() {
    // To look for history items visited in the last week,
    // subtract a week of microseconds from the current time.
    console.log(
      `collecting history for ${numberOfDays} days ${typeof numberOfDays}`
    );
    let millisecondsPerDays = 1000 * 60 * 60 * 24 * numberOfDays;
    let startTime = new Date().getTime() - millisecondsPerDays;

    // Track the number of callbacks from chrome.history.getVisits()
    // that we expect to get.  When it reaches zero, we have all results.
    let numRequestsOutstanding = 0;

    chrome.history.search(
      {
        text: '', // Return every history item....
        startTime, // that was accessed less than one week ago.
      },
      function (historyItems) {
        // For each history item, get details on all visits.
        for (let i = 0; i < historyItems.length; ++i) {
          let url = historyItems[i].url;
          if (url === undefined) {
            console.log('undefined url');
            continue;
          }
          let processVisitsWithUrl = function (url: string) {
            // We need the url of the visited item to process the visit.
            // Use a closure to bind the  url into the callback's args.
            return function (visitItems: chrome.history.VisitItem[]) {
              processVisits(url, visitItems);
            };
          };
          chrome.history.getVisits({ url: url }, processVisitsWithUrl(url));
          numRequestsOutstanding++;
        }
        if (!numRequestsOutstanding) {
          onAllVisitsProcessed();
        }
      }
    );

    // Maps URLs to a count of the number of times the user typed that URL into
    // the omnibox.
    let urlToCount: Record<string, number> = {};

    // Callback for chrome.history.getVisits().  Counts the number of
    // times a user visited a URL by typing the address.
    const processVisits = function (
      url: string,
      visitItems: chrome.history.VisitItem[]
    ) {
      for (let i = 0, ie = visitItems.length; i < ie; ++i) {
        // Ignore items unless the user typed the URL.
        if (visitItems[i].transition != 'typed') {
          continue;
        }

        if (!urlToCount[url]) {
          urlToCount[url] = 0;
        }

        urlToCount[url]++;
      }

      // If this is the final outstanding call to processVisits(),
      // then we have the final results.  Use them to build the list
      // of URLs to show in the popup.
      if (!--numRequestsOutstanding) {
        onAllVisitsProcessed();
      }
    };

    // This function is called when we have the final list of URls to display.
    const onAllVisitsProcessed = () => {
      // Get the top scorring urls.
      let urlArray = [];
      for (const [url, count] of Object.entries(urlToCount)) {
        urlArray.push({ url, count });
      }

      // Sort the URLs by the number of times the user typed them.
      urlArray.sort(function (a, b) {
        return b.count - a.count;
      });

      console.log(
        `Setting visited urls: ${urlArray
          .slice(0, 10)
          .map((x) => `${x.url} (${x.count})`)
          .join(', ')}`
      );
      setVisitedUrls(urlArray.slice(0, 10));
    };
  }

  const handleSubmitData = () => {
    console.log('submitting data');
    postData('http://localhost:3001/api/saveHistory', {
      data: visitedUrls,
      walletAddr: walletAddress,
    });
  };

  return (
    <main className="w-96 h-auto min-h-96 rounded-lg shadow-lg flex flex-col bg-zinc-50">
      {/* PERSON BUTTONS - ABSOLUTE POSITION */}
      <div className="flex flex-row justify-evenly gap-4 rounded-b-lg bg-zinc-800 w-full p-6">
        <button
          onClick={() => handleSubmitData()}
          className="bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)] font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
          disabled={visitedUrls.length === 0 || walletAddress === ''}
        >
          Submit Data
        </button>
      </div>

      <div className="flex flex-col items-center gap-8 h-full py-10 w-full rounded-2xl px-auto">
        <div className="flex flex-col w-full items-start gap-4">
          <div className="flex items-center gap-4">
            <label
              htmlFor="walletAddress"
              className="text-zinc-800 w-32 text-right"
            >
              Wallet Address:
            </label>
            <input
              type="text"
              id="walletAddress"
              value={walletAddress}
              onChange={(e) => setWalletAddress(e.target.value)}
              className="w-36 py-2 px-3 border rounded"
            />
          </div>
          <div className="flex items-center gap-4">
            <label htmlFor="days" className="text-zinc-800 w-32 text-right">
              Number of days:
            </label>
            <input
              type="number"
              id="days"
              value={numberOfDays}
              onChange={(e) => setNumberOfDays(+Number(e.target.value))}
              className="w-16 py-2 px-3 border rounded"
            />
          </div>
        </div>

        <button
          onClick={buildTypedUrlList}
          className="bg-zinc-100 text-zinc-800 py-2 px-4 rounded-lg"
        >
          Collect History
        </button>

        <ul className="w-full p-4">
          {visitedUrls.map((item, index) => (
            <li
              key={`${item}:${item.count}`}
              className="flex justify-between py-2 border-b"
            >
              <a
                href={item.url}
                target="_blank"
                rel="noopener noreferrer"
                className="text-blue-600 truncate"
              >
                {item.url}
              </a>
              <span className="text-zinc-800">Visits: {item.count}</span>
            </li>
          ))}
        </ul>
      </div>
    </main>
  );
};
export default Popup;
