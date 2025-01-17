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
import searchFormStyles from "./SearchForm.module.scss"
import bvoStyle from "../BvoStyle.module.scss"
import React from "react"
import classNames from "classnames"

export type SearchFormProps = {
    className?: string
    title: string
    children: React.ReactNode
}

const SearchForm = (props: SearchFormProps) => {

    return (
        <form className={classNames(bvoStyle.baseBackgroundColor, searchFormStyles.content, props.className)}
              autoComplete={"off"}>

            <div>
                <h2>{props.title}</h2>
            </div>

            <div>
                {props.children}
            </div>

        </form>
    )

}

export default SearchForm
