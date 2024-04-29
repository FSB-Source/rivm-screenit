import React from "react"
import DatePicker, {registerLocale} from "react-datepicker"
import moment from "moment"
import "react-datepicker/dist/react-datepicker.css"
import nl from "date-fns/locale/nl"
import {vandaagPlusDagen} from "../../util/DatePickerUtil"

registerLocale("nl", nl)
moment.updateLocale("nl", {
	months: ["januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktober", "november", "december"],
	weekdaysMin: ["zo", "ma", "di", "wo", "do", "vr", "za"],
	week: {
		dow: 1,
	},
})

export type DatumkiezerViewStateProps = {
	daglijstDatum: string;
	online: boolean;
	dagenDaglijstOphalenLimiet?: number;
};

export type DatumkiezerViewDispatchProps = {
	onChooseDate: (datum: string, online: boolean, limiet?: number) => void;
}

const DatumkiezerView = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): JSX.Element => {
	return <div className="row datumkiezer-div">
		<div className="col-10 row-no-gutters">
			{}
			<DatePicker
				className="datumkiezer-view clickable"
				locale="nl"
				value={getDatumkiezerValue(props)}
				highlightDates={getHighlightDates(props)}
				onChange={(newDate: Date): void => {
					props.onChooseDate(moment(newDate).format("YYYY-MM-DD"), props.online, props.dagenDaglijstOphalenLimiet)
				}}
				onKeyDown={(event: React.KeyboardEvent<HTMLDivElement>): void => event.preventDefault()}
			/>
		</div>
		<div className="col-2 datumkiezer-icon">
			<i className="fa fa-calendar px-1 py-1"/>
		</div>
	</div>
}

const getDatumkiezerValue = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): string => {
	moment.locale("nl")
	const datum = moment(props.daglijstDatum)
	return datum.format("dddd DD-MM-YYYY")
}

const getHighlightDates = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): Date[] => {
	const dates: Date[] = []
	if (props.dagenDaglijstOphalenLimiet) {
		for (let i = 0; i <= props.dagenDaglijstOphalenLimiet; i++) {
			dates.push(moment(vandaagPlusDagen(i)).toDate())
		}
	}
	return dates
}

export default DatumkiezerView