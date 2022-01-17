/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import React from "react"
import DateFnsUtils from "@date-io/date-fns"
import {KeyboardDatePicker, MuiPickersUtilsProvider} from "@material-ui/pickers"
import styles from "./ScreenitDatePicker.module.scss"
import {MaterialUiPickersDate} from "@material-ui/pickers/typings/date"
import {FormikErrors} from "formik"
import {getBovengrensUitLijst, getOndergrensUitLijst, lijstBevatMeegegevenDatum} from "../../utils/DateUtil"
import {nl} from "date-fns/locale"

export type ScreenitDatePickerProps = {
	className?: string,
	propertyName: string,
	title?: string,
	label: string,
	errorLabel?: string | FormikErrors<any>,
	value?: MaterialUiPickersDate,
	beschikbareDagen?: Date[]
	onChange: (date: MaterialUiPickersDate, value: string | undefined | null) => void
}

const ScreenitDatePicker = (props: ScreenitDatePickerProps) => {
	const ondergrens = getOndergrensUitLijst(props.beschikbareDagen)
	const bovengrens = getBovengrensUitLijst(props.beschikbareDagen)

	return (
		<div ref={React.createRef}>
			<p>{props.title}</p>
			<div className={props.errorLabel ? styles.inputDivError : styles.inputDiv}>
				<MuiPickersUtilsProvider locale={nl} utils={DateFnsUtils}>
					<KeyboardDatePicker
						data-testid={"input_" + props.propertyName}
						className={props.className}
						name={props.propertyName}
						label={props.label}
						variant={"inline"}
						format="dd-MM-yyyy"
						autoComplete={"off"}
						minDateMessage={""}
						maxDateMessage={""}
						shouldDisableDate={(date) => shouldDisableDate(date, ondergrens, bovengrens, props.beschikbareDagen)}
						autoOk={true}
						invalidDateMessage={""}
						disableToolbar
						value={props.value}
						onChange={props.onChange}
					/>
				</MuiPickersUtilsProvider>
				<p data-testid={"error_" + props.propertyName} className={styles.errorLabel}>{props.errorLabel}</p>
			</div>
		</div>
	)
}

export function shouldDisableDate(date: MaterialUiPickersDate, ondergrens?: Date, bovengrens?: Date, beschikbareDagen?: Date[]) {
	if (beschikbareDagen && beschikbareDagen.length > 0) {
		if (date !== null && ondergrens && bovengrens && date >= ondergrens && date <= bovengrens) {
			return !lijstBevatMeegegevenDatum(beschikbareDagen, date)
		} else {
			return true
		}
	} else {
		return false
	}
}

export default ScreenitDatePicker
