import React, {ReactNode} from "react"

interface BarcodeScannerState {
	maximaleInputtijdTussenCharsInMs: number,
	eindChar: number[],
	eersteCharScanTijd: number,
	laatsteCharScanTijd: number,
	gescandeString: string
}

interface BarcodeScannerProps {
	onScan: (input: string) => void,
	minimaleLengte: number,
}

class BarcodeReader extends React.Component<BarcodeScannerProps, BarcodeScannerState> {
	constructor(props: BarcodeScannerProps) {
		super(props)

		this.state = {
			maximaleInputtijdTussenCharsInMs: 30,
			eindChar: [9, 10, 13],
			eersteCharScanTijd: 0,
			laatsteCharScanTijd: 0,
			gescandeString: "",
		}
	}

	timeoutID: number | null = null

	componentDidMount(): void {
		window.document.addEventListener("keypress", this.handleKeyPress)
	}

	componentWillUnmount(): void {
		window.document.removeEventListener("keypress", this.handleKeyPress)
	}

	handleKeyPress = (keyPressEvent: KeyboardEvent): void => {
		if (this.state.eersteCharScanTijd && this.state.eindChar.indexOf(keyPressEvent.which) !== -1) {
			this.setState({laatsteCharScanTijd: Date.now()})
			keyPressEvent.preventDefault()
			keyPressEvent.stopImmediatePropagation()
			this.handleScan()
		} else {
			if (typeof (keyPressEvent.which) !== "undefined") {
				let nieuweString = this.state.gescandeString
				nieuweString += String.fromCharCode(keyPressEvent.which)
				this.setState({gescandeString: nieuweString})

				if (this.timeoutID !== null) {
					window.clearTimeout(this.timeoutID)
				}

				this.timeoutID = window.setTimeout(() => this.handleScan(), 100)
			}
		}

		if (!this.state.eersteCharScanTijd) {
			this.setState({eersteCharScanTijd: Date.now()})
		}
		this.setState({laatsteCharScanTijd: Date.now()})
	}

	initScannerDetectie = (): void => {
		this.setState({eersteCharScanTijd: 0})
		this.setState({gescandeString: ""})
	}

	render = (): ReactNode => {
		return null
	}

	handleScan = (): void => {
		if (this.timeoutID !== null) {
			window.clearTimeout(this.timeoutID)
		}

		const stringLangGenoeg = this.state.gescandeString.length >= this.props.minimaleLengte
		const inputSnelGenoeg = this.state.laatsteCharScanTijd - this.state.eersteCharScanTijd < this.state.gescandeString.length * this.state.maximaleInputtijdTussenCharsInMs
		if (stringLangGenoeg && inputSnelGenoeg) {
			this.props.onScan(this.state.gescandeString)
		}

		this.initScannerDetectie()
	}
}

export default BarcodeReader