1. Install Docker

2. Install Visual Studio Code

3. Install "Remote Development" extension

4. Click on the "Reopen in Container" button when opening the repo.

5. This step is critical as all the code and updates should run into the code directory.

```bash
cd code
```

6. Update all the dependencies required by Plutus.

```bash
cabal update
```

7.  Build all the dependencies required by Plutus.

```bash
cabal build all
```

After successfully running this command, you will see the system prompt back with no errors.

8. `cabal test all`

9. Build and load the browser extension

```bash
cd code/Draft/off-chain/extension

npm install

npm run start

# Load the build folder into chrome://extensions
```

### Common Issues

- node permission denied:
  Run `chown -R root:root . `

- npm: cannot find module bootloader
  Disable autto-atach debugging
