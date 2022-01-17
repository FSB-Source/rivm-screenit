module.exports = {
    webpack: function (config) {
        return {
            ...config,
            optimization: {
                ...config.optimization,
                minimize: false,
                minimizer: []
            },
        };
    },
}
