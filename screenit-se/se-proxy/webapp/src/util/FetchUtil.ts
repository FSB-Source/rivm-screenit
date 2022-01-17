const AbortController = window.AbortController

export const fetchWithTimeout = (url: string, timeoutMs: number, options: RequestInit): Promise<Response> => {
	const controller = new AbortController()
	const fetchPromise = fetch(url, {
		signal: controller.signal,
		...options,
	})
	const timeoutId = setTimeout(() => {
		console.error(`Timeout van ${timeoutMs}ms opgetreden op url: ${url}`)
		controller.abort()
	}, timeoutMs)
	return fetchPromise.finally(() => clearTimeout(timeoutId))
}