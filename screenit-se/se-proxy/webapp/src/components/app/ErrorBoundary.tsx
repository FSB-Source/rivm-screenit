import React, {ErrorInfo} from "react"
import {store} from "../../Store"
import {verstuurConsoleMeldingNaarCentraal} from "../../restclient/ErrorRestClient"
import {navigateToDaglijst} from "../../util/NavigationUtil"

type ErrorBoundaryProps = {
	children: any;
}

type ErrorBoundaryState = {
	hasError: boolean;
}

export default class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
	constructor(props: ErrorBoundaryProps) {
		super(props)
		this.state = {hasError: false}
	}

	componentDidCatch(error: Error, info: ErrorInfo): void {
		verstuurConsoleMeldingNaarCentraal({
			level: "ERROR",
			melding: `Fout uit component: ${error.message}`,
			stack: info.componentStack,
		})
		this.setState({hasError: true})
	}

	render(): JSX.Element {
		if (this.state.hasError) {
			navigateToDaglijst(store.dispatch)
		}
		return this.props.children
	}
}

(function (): void {

	const oldConsole: any = console

	const oldTrace = oldConsole.trace
	oldConsole.trace = (message: string): void => {
		verstuurConsoleMeldingNaarCentraal({
			level: "TRACE",
			melding: message,
		})
		oldTrace.apply(oldConsole, arguments)
	}

	const oldLog = oldConsole.log
	oldConsole.log = (message: string): void => {
		verstuurConsoleMeldingNaarCentraal({
			level: "LOG",
			melding: message,
		})
		oldLog.apply(oldConsole, arguments)
	}

	const oldWarn = oldConsole.warn
	oldConsole.warn = (message: string): void => {
		verstuurConsoleMeldingNaarCentraal({
			level: "WARN",
			melding: message,
		})
		oldWarn.apply(oldConsole, arguments)
	}

	const oldError = oldConsole.error
	oldConsole.error = (message: string): void => {
		verstuurConsoleMeldingNaarCentraal({
			level: "ERROR",
			melding: message,
		})
		oldError.apply(oldConsole, arguments)
	}

	window.addEventListener("error", (e) => {
		console.error(`Error occurred: ${e.error.stack}`)
	})

	window.addEventListener("unhandledrejection", (e) => {
		console.error(`Unhandled rejection: ${e.reason.stack}`)
	})

})()
