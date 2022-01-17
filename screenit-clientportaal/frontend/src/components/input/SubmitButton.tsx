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
import {ArrowType} from "../vectors/ArrowIconComponent"
import {Formik} from "formik"
import Button from "./Button"

export type SubmitButtonProps = {
	className?: string
	label: string,
	displayArrow?: ArrowType,
	arrowBeforeLabel?: boolean,
	onClick: () => void,
	lightStyle?: boolean
}

const SubmitButton = (props: SubmitButtonProps) => {

    return (
        <Formik initialValues={{}}
                onSubmit={props.onClick}>
			{formikProps => (
				<Button className={props.className}
						disableButton={formikProps.isSubmitting}
						label={props.label}
						displayArrow={props.displayArrow}
						arrowBeforeLabel={props.arrowBeforeLabel}
						onClick={formikProps.handleSubmit}
						lightStyle={props.lightStyle}/>)}
        </Formik>
    )

}

export default SubmitButton
