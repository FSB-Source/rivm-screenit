import {store} from "../Store"

export const getTokenHeader = (): Headers => {
	const myHeaders = new Headers()
	const session = store.getState().session
	if (session?.sessionId) {
		myHeaders.append("x-auth-token", session.sessionId)
	}
	return myHeaders
}

export const getToken = (): string | undefined => {
	return store.getState().session?.sessionId
}