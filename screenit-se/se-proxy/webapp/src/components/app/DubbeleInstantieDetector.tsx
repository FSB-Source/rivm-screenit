import React from "react"
import {dispatchActions} from "../../util/DispatchUtil"
import {store} from "../../Store"
import {createActionDubbeleInstantie} from "../../actions/DubbeleInstantieActions"
import {MELDING_DUBBELE_INSTANTIE, showErrorToastWithoutAutoClose} from "../../util/ToastUtil"

const SE_VENSTER_GEOPEND = "SE_VENSTER_GEOPEND"
const SE_SLUIT_DUBBEL_VENSTER = "SE_SLUIT_DUBBEL_VENSTER"

export default class DubbeleInstantieDetector extends React.Component {

	constructor(props: never) {
		super(props)
		this.handlelocalStorageUpdated = this.handlelocalStorageUpdated.bind(this)
	}

	componentDidMount(): void {
		localStorage.setItem(SE_VENSTER_GEOPEND, Date.now().toString())
		window.addEventListener("storage", this.handlelocalStorageUpdated)
	}

	handlelocalStorageUpdated = (event: any): void => {
		console.log(`Local storage event: ${event.key}`)

		if (event.key === SE_VENSTER_GEOPEND) {
			localStorage.setItem(SE_SLUIT_DUBBEL_VENSTER, Date.now().toString())
		}

		if (event.key === SE_SLUIT_DUBBEL_VENSTER) {
			this.removeEventListener()

			dispatchActions(store.dispatch, createActionDubbeleInstantie())
			showErrorToastWithoutAutoClose(MELDING_DUBBELE_INSTANTIE)
		}
	}

	removeEventListener(): void {
		window.removeEventListener("storage", this.handlelocalStorageUpdated)
	}

	componentWillUnmount(): void {
		this.removeEventListener()
	}

	render(): JSX.Element | null {
		return null
	}

}