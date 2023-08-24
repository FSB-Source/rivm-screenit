package nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public abstract class MammaUploadBeeldenVerzoekAnnulerenPanel extends Panel
{
	public MammaUploadBeeldenVerzoekAnnulerenPanel(String id, BootstrapDialog bootstrapDialog, String resourceKey, IModel<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoekModel)
	{
		super(id, uploadBeeldenVerzoekModel);
		add(new ConfirmingIndicatingAjaxLink<Void>("annuleerVerzoek", bootstrapDialog, resourceKey)
		{
			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				onOpslaan(ajaxRequestTarget, (MammaUploadBeeldenVerzoek) MammaUploadBeeldenVerzoekAnnulerenPanel.this.getDefaultModelObject());
			}
		});
	}

	abstract protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek);
}
