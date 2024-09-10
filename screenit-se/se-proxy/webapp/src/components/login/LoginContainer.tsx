import {connect} from "react-redux"
import type {LoginProps} from "./LoginView"
import LoginView from "./LoginView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): LoginProps => ({
	dubbeleInstantie: state.dubbeleInstantie,
	inlogActief: state.loginStatus.inlogActief,
	isTestOmgeving: state.environmentInfo ? state.environmentInfo.environment === "Test" : false,
	websocketStatus: state.websocketStatus,
})

const LoginContainer = connect(mapStateToProps)(LoginView)
export default LoginContainer