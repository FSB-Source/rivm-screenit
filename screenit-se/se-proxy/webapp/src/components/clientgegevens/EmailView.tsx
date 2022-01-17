import React, {Component} from "react"
import PaneelNaam from "../generic/PaneelNaam"
import Paneel from "../generic/Paneel"
import type {Form, FORM_FIELD_ID} from "../../datatypes/Form"
import ValidationInputContainer from "../generic/ValidationInputContainer"
import {Col, Row} from "reactstrap"

export type EmailViewStateProps = {
	clientId: number;
	emailadres?: string;
	disabled: boolean;
	clientGegevensForm: Form;
};

export type EmailViewDispatchProps = {
	setEmailAdres: (clientId: number, emailadres: string) => void;
}

export const EMAIL_FIELD_ID: FORM_FIELD_ID = {
	formId: "clientgegevens",
	fieldId: "email",
}

export default class EmailView extends Component<EmailViewStateProps & EmailViewDispatchProps> {
	constructor(props: EmailViewStateProps & EmailViewDispatchProps) {
		super(props)
		this.onEmailChange = this.onEmailChange.bind(this)
	}

	onEmailChange = (value: string): void => {
		if (value !== this.props.emailadres) {
			this.props.setEmailAdres(this.props.clientId, value)
		}
	}

	render(): JSX.Element {
		return <Paneel>
			<PaneelNaam titel={"E-mailadres"}/>
			<div className={"form-row"}>
				<Col>
					<Row noGutters>
						<Col md={2}>
							E-mailadres
						</Col>
						<Col md={3}>
							<ValidationInputContainer
								label={"E-mailadres"} placeholder={"Voer e-mailadres in"}
								disabled={this.props.disabled} value={this.props.emailadres || ""}
								onChange={this.onEmailChange} fieldId={EMAIL_FIELD_ID}
								maxLength={50}/>
						</Col>
					</Row>
				</Col>
			</div>
		</Paneel>
	}

}