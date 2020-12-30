
package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.BezwaarMoment;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.RepeatingView;

public abstract class ClientContactAfrondenPopupPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private final BootstrapDialog dialog;

	public ClientContactAfrondenPopupPanel(String id, List<String> meldingen)
	{
		super(id);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		Form<BezwaarMoment> form = new ScreenitForm<>("uploadForm");
		add(form);

		RepeatingView actie = new RepeatingView("actie");
		for (String melding : meldingen)
		{
			actie.add(new Label(actie.newChildId(), melding));
		}
		form.add(actie);

		form.add(new ScreenitIndicatingAjaxSubmitLink("doorgaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target);
			}

		});
	}

	protected abstract void opslaan(AjaxRequestTarget target);
}
