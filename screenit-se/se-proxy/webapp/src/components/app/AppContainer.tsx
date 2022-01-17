import {connect} from "react-redux"
import type {AppProps} from "./AppView"
import AppView from "./AppView"
import {clearWerklijst} from "../../restclient/WerklijstRestclient"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): AppProps => ({
	session: state.session || undefined,
	mammograafId: state.huidigeMammograafId !== null ? state.huidigeMammograafId : undefined,
	mammografenById: state.mammografenById || undefined,
})

const AppContainer = connect(mapStateToProps)(AppView)

window.onbeforeunload = function (): boolean {
	return false
}

window.onunload = function (): void {
	clearWerklijst()
}

export default AppContainer