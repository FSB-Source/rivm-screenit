/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {FormikProps} from "formik"
import React, {ChangeEvent} from "react"
import {WoonplaatsDto} from "../../../state/datatypes/dto/WoonplaatsDto"
import styles from "./FormWoonplaatsSelectField.module.scss"
import AsyncSelect from "react-select/async"
import {AxiosResponse} from "axios"
import ScreenitBackend from "../../../util/Backend"
import properties from "./FormWoonplaatsSelectField.json"
import {getString} from "../../../util/TekstPropertyUtil"

export interface FormWoonplaatsSelectFieldProps<T> {
	form: FormikProps<T>;
	selectedValue: WoonplaatsDto;
	property: string;
	disabled?: boolean;
	handleChange: (e: ChangeEvent<any>) => void;
}

const loadWoonplaatsen = async (value: string): Promise<WoonplaatsDto[]> => {
	return await ScreenitBackend.get("/woonplaatsen/" + value).then((response: AxiosResponse<WoonplaatsDto[]>) => {
		return response.data
	})
}

function FormWoonplaatsSelectField<T>(props: FormWoonplaatsSelectFieldProps<T>) {
	return <div className={styles.style}>
		<AsyncSelect
			styles={undefined}
			defaultOptions
			value={!!props.selectedValue && props.selectedValue}
			isDisabled={!!props.disabled}
			isClearable
			placeholder={getString(properties.placeholder)}
			noOptionsMessage={() => getString(properties.geenOpties)}
			loadingMessage={() => getString(properties.laden)}
			getOptionLabel={v => v ? (v.naam + " (Gemeente " + v.gemeente + ")") : ""}
			getOptionValue={v => v ? String(v.huisartsportaalId) : ""}
			loadOptions={(value: string) => {
				return (value.length > 2 ? loadWoonplaatsen(value) : new Promise((resolve) => resolve([])))
			}}
			onChange={(value) => {
				props.form.setFieldValue(props.property, value)
			}}
		/>
	</div>
}

export default FormWoonplaatsSelectField
