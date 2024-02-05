/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import styles from "./SubmitForm.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import React from "react"
import Button from "../input/Button"
import {useSelectedBvo} from "../../utils/Hooks"
import classNames from "classnames"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import {ArrowType} from "../vectors/ArrowIconComponent"
import VerticalDividerComponent from "../vectors/VerticalDividerComponent"
import {FormikProps} from "formik"

export type FormProps<T> = {
    className?: string
    title: string
    formikProps: FormikProps<T>
    buttonLabel: string
    children: React.ReactNode
}

const SubmitForm = <T extends any>(props: FormProps<T>) => {
    const selectedBvo = useSelectedBvo()

    return (
        <form className={classNames(props.className, styles.content, selectedBvo && BevolkingsonderzoekStyle[selectedBvo])}>
            <VerticalDividerComponent className={styles.verticalRectangle}/>
            <div>
                <h3>{props.title}</h3>
            </div>
            <div>
                {props.children}
            </div>
            <Button className={classNames(bvoStyle.baseBackgroundColor, styles.submitButton)}
                    disableButton={props.formikProps.isSubmitting}
                    label={props.buttonLabel}
                    displayArrow={ArrowType.ARROW_RIGHT}
                    onClick={props.formikProps.handleSubmit}/>
        </form>
    )
}

export default SubmitForm
