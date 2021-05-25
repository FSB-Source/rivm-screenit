package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.service.ClientService;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class BevestigVerwijderClientDossierPopupPanel extends GenericPanel<BezwaarMoment>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	public BevestigVerwijderClientDossierPopupPanel(String id, boolean bezwaarBRP, boolean clientDossierVerwijderen)
	{
		super(id);

		Form<BezwaarMoment> uploadForm = new ScreenitForm<>("uploadForm");
		add(uploadForm);

		uploadForm.add(new AjaxSubmitLink("bevestig")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target);
			}
		});

		Component bezwaarBRPComponent = new EmptyPanel("bezwaarBRP");
		if (bezwaarBRP)
		{
			bezwaarBRPComponent = new Label("bezwaarBRP", getString("bezwaar.waarschuwing.bezwaarbrp"));
		}
		Component clientDossierVerwijderenComponent = new EmptyPanel("clientDossierVerwijderen");
		if (clientDossierVerwijderen)
		{
			clientDossierVerwijderenComponent = new Label("clientDossierVerwijderen", getString("bezwaar.waarschuwing.verwijderendossier"));
		}
		uploadForm.add(bezwaarBRPComponent);
		uploadForm.add(clientDossierVerwijderenComponent);
	}

	protected abstract void opslaan(AjaxRequestTarget target);

}
