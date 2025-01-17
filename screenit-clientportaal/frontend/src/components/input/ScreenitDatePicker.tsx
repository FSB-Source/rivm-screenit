/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {DatePicker, LocalizationProvider} from "@mui/x-date-pickers"
import {TextField} from "@mui/material"
import styles from "./ScreenitDatePicker.module.scss"
import {FormikErrors} from "formik"
import {getBovengrensUitLijst, getOndergrensUitLijst, isWerkdag, lijstBevatMeegegevenDatum} from "../../utils/DateUtil"
import {AdapterDateFns} from "@mui/x-date-pickers/AdapterDateFns"
import {nl} from "date-fns/locale"

export type ScreenitDatePickerProps = {
	className?: string,
	propertyName: string,
	title?: string,
	label: string,
	errorLabel?: string | FormikErrors<any>,
	value?: Date | null,
	beschikbareDagen?: Date[],
	alleenWerkdagen?: boolean,
	onChange: (date: Date | null, value: string | undefined | null) => void
}

const ScreenitDatePicker = (props: ScreenitDatePickerProps) => {

	const ondergrens = getOndergrensUitLijst(props.beschikbareDagen)
	const bovengrens = getBovengrensUitLijst(props.beschikbareDagen)

	return (
		<div ref={React.createRef}>
			<p>{props.title}</p>
			<div className={props.errorLabel ? styles.inputDivError : styles.inputDiv}>
				<LocalizationProvider adapterLocale={nl} dateAdapter={AdapterDateFns}>
					<DatePicker
						views={["day"]}
						className={props.className}
						inputFormat="dd-MM-yyyy"
						label={props.label}
						value={props.value}
						onChange={props.onChange}
						shouldDisableDate={(date) => shouldDisableDate(date, ondergrens, bovengrens, props.beschikbareDagen, props.alleenWerkdagen)}
						renderInput={(params) => <TextField data-testid={"input_" + props.propertyName}
															variant="standard" {...params}  />}
					/>

				</LocalizationProvider>
				<p data-testid={"error_" + props.propertyName} className={styles.errorLabel}>{props.errorLabel && String(props.errorLabel)}</p>
			</div>
		</div>
	)
}

export function shouldDisableDate(date: Date | null, ondergrens?: Date, bovengrens?: Date, beschikbareDagen?: Date[], alleenWerkdagen?: boolean) {
	if (date !== null && alleenWerkdagen && !isWerkdag(date)) {
		return true
	}
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
