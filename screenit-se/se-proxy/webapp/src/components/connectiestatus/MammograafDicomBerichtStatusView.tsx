import React, {Component} from "react"
import type {ConnectieStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"
import {getTijdGeledenTekst} from "../../util/DateUtil"
import {MammograafDicomMessageType} from "../../datatypes/connectiestatus/MammograafDicomMessageError"

export type MammograafDicomBerichtStatusStateProps = {
	messageType: MammograafDicomMessageType;
	level: ConnectieStatusLevel;
	timestamp?: string;
};

export type MammograafDicomBerichtStatusDispatchProps = {
	onClick: () => void;
};

export default class MammograafDicomBerichtStatusView extends Component<MammograafDicomBerichtStatusStateProps & MammograafDicomBerichtStatusDispatchProps> {
	render(): JSX.Element {
		switch (this.props.level) {
			case "FAULT":
				return <p><span>Er zijn <u onClick={this.props.onClick}><b>fouten</b></u> opgetreden </span>
					{this.props.timestamp ?
						<span>na het laatst succesvolle {this.props.messageType} bericht van: <b>{getTijdGeledenTekst(this.props.timestamp)}</b></span> :
						<span>bij het eerste {this.props.messageType} bericht sinds het opstarten</span>}</p>

			case "OK":
				return <p>Laatst ontvangen
					succesvolle {this.props.messageType} bericht: <b>{this.props.timestamp ? getTijdGeledenTekst(this.props.timestamp) : "Onbekend"}</b>
				</p>

			case "WARN":
				return <p>Er is nog geen succesvol {this.props.messageType} bericht ontvangen</p>
		}
	}

}