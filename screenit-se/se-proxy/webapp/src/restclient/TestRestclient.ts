import {fetchApi} from "../util/ApiUtil"

export const gaOffline = (): void => {
	fetchApi("POST", "test/offline")
}

export const gaOnline = (): void => {
	fetchApi("POST", "test/online")
}