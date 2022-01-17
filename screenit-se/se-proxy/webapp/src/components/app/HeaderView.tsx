import React from "react"
import {Button, Col, Row} from "reactstrap"
import {Mammograaf} from "../../datatypes/Mammograaf"
import {gaOffline, gaOnline} from "../../restclient/TestRestclient"

export type HeaderViewStateProps = {
	aangemeld: boolean;
	gebruikersnaam?: string;
	displayName?: string;
	seCode?: string;
	seNaam?: string;
	huidigeMammograaf?: Mammograaf;
	online: boolean;
	isTestOmgeving: boolean;
};

export type HeaderViewDispatchProps = {
	afmelden: () => void;
}

export default class HeaderView extends React.Component<HeaderViewStateProps & HeaderViewDispatchProps> {
	render(): JSX.Element | null {
		return this.props.aangemeld ? <Col md={10} className={"float-right gebruiker-view"}>
			{!this.props.online ?
				<img className="geen-verbinding" src="images/geen-verbinding.png" alt="geen-verbinding"/> : null}
			{this.props.isTestOmgeving && (this.props.online ? <div className="col-2 offline-knop">
				<input className="btn btn-danger" type="submit" value="Ga offline" onClick={(): void => {
					gaOffline()
				}}/>
			</div> : <div className="col-2 offline-knop">
				<input className="btn btn-success" type="submit" value="Ga online" onClick={(): void => {
					gaOnline()
				}}/>
			</div>)}
			<Row>
				<Col md={5}>
					<div className={"float-right"}>
						{this.props.displayName}
					</div>
				</Col>
				<Col md={3}>{this.props.seNaam}</Col>
				<Col md={2}>{this.props.huidigeMammograaf ? this.props.huidigeMammograaf.aeTitle : ""}</Col>
				<Col md={2}>
					<Button
						id="afmeldButton" color={"link"} className={"btn mr-1 white-link-button btn-sm"}
						onClick={(): void => {
							this.props.afmelden()
						}}> Afmelden </Button>
				</Col>
			</Row>
		</Col> : this.props.isTestOmgeving ? (this.props.online ?
			<Col md={10} className={"float-right gebruiker-view"}>
				<div className="col-2 offline-knop">
					<input className="btn btn-danger" type="submit" value="Ga offline" onClick={(): void => {
						gaOffline()
					}}/>
				</div>
			</Col> : <Col md={10} className={"float-right gebruiker-view"}>
				<div className="col-2 offline-knop">
					<input className="btn btn-success" type="submit" value="Ga online" onClick={(): void => {
						gaOnline()
					}}/>
				</div>
			</Col>) : null
	}

}