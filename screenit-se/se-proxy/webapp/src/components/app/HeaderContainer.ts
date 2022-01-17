import {connect} from "react-redux"
import HeaderView, {HeaderViewDispatchProps, HeaderViewStateProps} from "./HeaderView"
import {logout} from "../../restclient/AuthenticatieRestclient"
import {RootState, store} from "../../Store"

const mapStateToProps = (state: RootState): HeaderViewStateProps => {
	return state.session ? {
		aangemeld: true,
		gebruikersnaam: state.session.gebruikersnaam,
		displayName: state.session.displayName,
		seCode: state.session.seCode,
		seNaam: state.session.seNaam,
		huidigeMammograaf: state.huidigeMammograafId ? state.mammografenById.get(state.huidigeMammograafId) : undefined,
		online: state.online,
		isTestOmgeving: state.environmentInfo ? state.environmentInfo.environment === "Test" : false,
	} : {
		aangemeld: false,
		isTestOmgeving: state.environmentInfo ? state.environmentInfo.environment === "Test" : false,
		online: state.online,
	}
}

const mapDispatchToProps = (): HeaderViewDispatchProps => ({
	afmelden(): void {
		console.log(`Uitloggen via afmeldknop: ${store.getState().session?.yubikeyIdentificatie}`)
		logout(store.getState().session?.yubikeyIdentificatie)
	},
})

const HeaderContainer = connect(mapStateToProps, mapDispatchToProps)(HeaderView)
export default HeaderContainer