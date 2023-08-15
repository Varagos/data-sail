const path = require("path");
const HTMLPlugin = require("html-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin")

module.exports = {
    entry: {
        index: "./src/index.tsx",
        sandbox: "./src/sandbox.tsx"
    },
    mode: 'production',
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                use: [
                    {
                        loader: "ts-loader",
                        options: {
                            compilerOptions: { noEmit: false },
                        }
                    }],
                exclude: /node_modules/,
            },
            {
                test: /\.css$/,
                exclude: /node_modules/,
                use: [
                    "style-loader",
                    {
                        loader: "css-loader",
                        options: { importLoaders: 1 },
                    },
                    "postcss-loader",
                ],
            },
        ],
    },
    plugins: [
        new CopyPlugin({
            patterns: [
                { from: "manifest.json", to: "../manifest.json" },
                { from: "assets", to: "../assets" },
            ],
        }),
        // Use HtmlWebpackPlugin to generate an HTML file for the popup and sandboxed pages
        ...getHtmlPlugins(["index", "sandbox"]),
    ],
    resolve: {
        extensions: [".tsx", ".ts", ".js"],
    },
    output: {
        path: path.join(__dirname, "dist/js"),
        filename: "[name].js",
    },
    experiments: {
        asyncWebAssembly: true,
        topLevelAwait: true,
        layers: true // optional, with some bundlers/frameworks it doesn't work without
    }
};

function getHtmlPlugins(chunks) {
    return chunks.map(
        (chunk) =>
            new HTMLPlugin({
                title: "React extension",
                filename: `${chunk}.html`,
                chunks: [chunk],
            })
    );
}