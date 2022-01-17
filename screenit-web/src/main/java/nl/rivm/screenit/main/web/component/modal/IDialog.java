
package nl.rivm.screenit.main.web.component.modal;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;

public interface IDialog
{

	String CONTENT_ID = "content";

	void open(AjaxRequestTarget target);

	void setContent(Component content);

	void onClose(AjaxRequestTarget target);

	void close(AjaxRequestTarget target);

	void setCloseCallback(IDialogCloseCallback iDialogCloseCallback);

}
