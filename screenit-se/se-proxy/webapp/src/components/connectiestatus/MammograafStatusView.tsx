import React from "react"
import type {MammograafStatus} from "../../datatypes/connectiestatus/MammograafStatus"
import type {ConnectieStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"
import {connectieStatusLevels, getMostCriticalStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"
import type {ConnectieStatusItem} from "./ConnectieStatusItem"
import moment from "moment"
import {Col, Row} from "reactstrap"
import MammograafDicomBerichtStatusView from "./MammograafDicomBerichtStatusView"
import type {MammograafDicomMessageType} from "../../datatypes/connectiestatus/MammograafDicomMessageError"
import {nu} from "../../util/DateUtil"

export type MammograafStatusViewStateProps = {
	mammograafStatus: MammograafStatus;
	mammograafConnectieStatusLevel: ConnectieStatusLevel;
};

export type MammograafStatusViewDispatchProps = {
	reportStatusLevel: (aeTitle: string, statusLevel: ConnectieStatusLevel) => void;
	toonDicomFouten: (messageType: MammograafDicomMessageType, mammograafStatus: MammograafStatus) => void;
}

export default class MammograafStatusView extends React.Component<MammograafStatusViewStateProps & MammograafStatusViewDispatchProps> implements ConnectieStatusItem {

	constructor(props: MammograafStatusViewStateProps & MammograafStatusViewDispatchProps) {
		super(props)
		this.getMppsStatusLevel.bind(this)
		this.getDmwlStatusLevel.bind(this)
		this.getDatumStatusLevel.bind(this)
		this.getStatusLevel.bind(this)
	}

	render(): JSX.Element {
		const mammograafStatus: MammograafStatus = this.props.mammograafStatus
		return <Row className={"connectiestatus-subitem"}>
			<Col md={12}>
				<Row>
					<i className={connectieStatusLevels[this.getStatusLevel()]}/>
					<h1>{mammograafStatus.aeTitle}</h1>
				</Row>
				<Row className={"connectiestatus-leaf"}>
					<Col>
						<i className={connectieStatusLevels[this.getMppsStatusLevel()]}/>
						<MammograafDicomBerichtStatusView messageType={"MPPS"} level={this.getMppsStatusLevel()}
														  timestamp={mammograafStatus.laatsteSuccesMppsBerichtTimestamp}
														  onClick={(): void => this.props.toonDicomFouten("MPPS", mammograafStatus)}/>
					</Col>
				</Row>
				<Row className={"connectiestatus-leaf"}>
					<Col>
						<i className={connectieStatusLevels[this.getDmwlStatusLevel()]}/>
						<MammograafDicomBerichtStatusView messageType={"DMWL"} level={this.getDmwlStatusLevel()}
														  timestamp={mammograafStatus.laatsteSuccesDmwlBerichtTimestamp}
														  onClick={(): void => this.props.toonDicomFouten("DMWL", mammograafStatus)}/>
					</Col>
				</Row>
				<Row className={"connectiestatus-leaf"}>
					<Col>
						<i className={connectieStatusLevels[this.getDatumStatusLevel()]}/>
						<p>{this.getDatumStatusLevel() === "OK" ? "Datum mammograaf komt overeen met ScreenIT" : this.getDatumStatusLevel() === "FAULT" ? "Datum mammograaf komt niet overeen met ScreenIT" : "Datum mammograaf is nog niet ontvangen"}</p>
					</Col>
				</Row>
			</Col>
		</Row>
	}

	getMppsStatusLevel = (): ConnectieStatusLevel => {
		return this.props.mammograafStatus.foutenSindsLaatsteSuccesMppsBericht.length > 0 ? "FAULT" : this.props.mammograafStatus.laatsteSuccesMppsBerichtTimestamp ? "OK" : "WARN"
	}
	getDmwlStatusLevel = (): ConnectieStatusLevel => {
		return this.props.mammograafStatus.foutenSindsLaatsteSuccesDmwlBericht.length > 0 ? "FAULT" : this.props.mammograafStatus.laatsteSuccesDmwlBerichtTimestamp ? "OK" : "WARN"
	}
	getDatumStatusLevel = (): ConnectieStatusLevel => {
		return this.props.mammograafStatus.mammograafDatum ? moment(this.props.mammograafStatus.mammograafDatum).isSame(nu(), "day") ? "OK" : "FAULT" : "WARN"
	}
	getStatusLevel = (): ConnectieStatusLevel => {
		const statusLevel = getMostCriticalStatusLevel([this.getDmwlStatusLevel(), this.getMppsStatusLevel(), this.getDatumStatusLevel()])

		if (statusLevel !== this.props.mammograafConnectieStatusLevel && this.props.mammograafStatus.aeTitle) {
			this.props.reportStatusLevel(this.props.mammograafStatus.aeTitle, statusLevel)
		}

		return statusLevel
	}
}