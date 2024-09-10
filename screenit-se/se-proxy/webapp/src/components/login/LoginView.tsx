import React, {ChangeEvent, Component, FormEvent} from "react"
import {Spinner} from "reactstrap"
import {login} from "../../restclient/AuthenticatieRestclient"
import {NfcOtpResponse, readNFC} from "../../restclient/NfcRestClient"
import {dismissAllToasts, persistentErrorToast} from "../../util/ToastUtil"
import {nuTimestamp} from "../../util/DateUtil"
import classNames from "classnames"
import {WEBSOCKET_STATUS_OFFLINE, WebsocketStatus} from "../../datatypes/WebsocketStatus"

export type LoginProps = {
	dubbeleInstantie: boolean;
	inlogActief: boolean;
	isTestOmgeving: boolean;
	websocketStatus: WebsocketStatus
};

export default class LoginView extends Component<LoginProps, any> {

	constructor(props: LoginProps) {
		super(props)
		this.state = {
			username: "",
			password: "",
			passwordVisible: false,
		}
		this.handleChange = this.handleChange.bind(this)
		this.handleSubmit = this.handleSubmit.bind(this)
		this.handleTogglePasswordVisible = this.handleTogglePasswordVisible.bind(this)
	}

	handleChange = (event: ChangeEvent<HTMLInputElement>): void => {
		this.setState({
			[event.target.name]: event.target.value,
		})
	}

	handleSubmit = (event: FormEvent<HTMLFormElement>): void => {
		event.preventDefault()
		const username = this.state.username.trim()
		const password = this.state.password

		if (username.length === 0) {
			persistentErrorToast("Gebruikersnaam is niet ingevuld.")
		} else if (password.length === 0) {
			persistentErrorToast("Wachtwoord is niet ingevuld.")
		} else {
			dismissAllToasts()
			console.log(`${nuTimestamp()}: Gedrukt op inloggen`)
			readNFC().then((responseData: NfcOtpResponse) => {
				const nfcTag = responseData.public_id
				const yubikey = responseData.otp
				login(username, password, nfcTag, yubikey)
			}).catch((error) => {
				console.error(`${nuTimestamp()}: Fout tijdens inloggen: ${error}`)
				persistentErrorToast("Fout tijdens inloggen")
			})
		}
	}

	handleTogglePasswordVisible = (): void => {
		this.setState({
				...this.state,
				passwordVisible: !this.state.passwordVisible,
			},
		)
	}

	render(): JSX.Element {
		return <form className="login" onSubmit={this.handleSubmit}>
			<Spinner className={this.props.inlogActief ? "login-spinner" : "login-spinner-disabled"}/>
			{this.props.inlogActief ? <div className={"laad-block-grijs"}/> : null}
			<div className="form-group row no-gutters">
				<div className="offset-3 col-2">
					<label htmlFor="gebruikersnaam" className="col-11 col-form-label text-right">Gebruikersnaam</label>
				</div>
				<div className="col-2">
					<input id="gebruikersnaam" autoComplete="off" className="" type="text" name="username"
						   onChange={this.handleChange} autoFocus disabled={this.props.dubbeleInstantie}/>
				</div>
			</div>
			<div className="form-group row no-gutters">
				<div className="offset-3 col-2">
					<label htmlFor="wachtwoord" className="col-11 col-form-label text-right">Wachtwoord</label>
				</div>
				<div className="col-2">
					<div className={"wachtwoord-input"}>
						<input id="wachtwoord" type={this.state.passwordVisible ? "text" : "password"} name="password" autoComplete="off"
							   onChange={this.handleChange} disabled={this.props.dubbeleInstantie}/>
						<i className={classNames("fa", this.state.passwordVisible ? "fa-eye-slash" : "fa-eye")} title={"Houd het oogje ingedrukt om wachtwoord te tonen"}
						   onMouseLeave={(): void => {
							   if (this.state.passwordVisible) {
								   this.handleTogglePasswordVisible()
							   }
						   }}
						   onMouseDown={this.handleTogglePasswordVisible} onMouseUp={this.handleTogglePasswordVisible}/>
					</div>
				</div>
			</div>
			<div className="form-group row no-gutters">
				<div className="offset-5 col-2">
					<input
						className={this.props.dubbeleInstantie || this.props.inlogActief ? "btn-primary-se disabled" : "btn btn-success"}
						type="submit" value="Inloggen"
						disabled={this.props.dubbeleInstantie || this.props.inlogActief || (this.props.isTestOmgeving && this.props.websocketStatus === WEBSOCKET_STATUS_OFFLINE)}/>
				</div>
			</div>
		</form>
	}

}