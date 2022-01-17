export function getMandatory<K, V>(map: Map<K, V>, key?: K): V {
	if (map && map instanceof Map) {
		if (key) {
			const val: V | void = map.get(key)
			if (val) {
				return val
			}
			throw new Error(`Map didn't return an expected value(-type) [key=${key}]`)
		}
		throw new Error(`Not a valid key: ${key}`)
	}
	throw new Error(`Supplied map isn't valid: ${map}`)
}

export function getIfExists<K, V>(map: Map<K, V>, key?: K): V | undefined {
	if (map && map instanceof Map) {
		if (key) {
			const val: V | void = map.get(key)
			if (val) {
				return val
			}
		}

		return undefined
	}
	console.error(`Supplied map isn't valid: ${map} [key=${key}]`)
	return undefined
}