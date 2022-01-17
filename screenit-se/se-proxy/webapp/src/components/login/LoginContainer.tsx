import {connect} from "react-redux"
import type {LoginProps} from "./LoginView"
import LoginView from "./LoginView"
import {RootState} from "../../Store"

const mapStateToProps = (state: RootState): LoginProps => ({
	dubbeleInstantie: state.dubbeleInstantie,
	inlogActief: state.loginStatus.inlogActief,
})

const LoginContainer = connect(mapStateToProps)(LoginView)
export default LoginContainer