package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.mamma.MammaMammograafService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

abstract class MammaMammograafEditPanel extends GenericPanel<MammaMammograaf>
{
	@SpringBean
	private MammaMammograafService mammaMammograafService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private HibernateService hibernateService;

	MammaMammograafEditPanel(final String id, final IModel<MammaMammograaf> model)
	{
		super(id, model);
		Form<MammaMammograaf> form = new ScreenitForm<>("form", model);
		add(form);

		final Long mammograafId = model.getObject().getId();
		ComponentHelper.addTextField(form, "aeTitle", true, 15, String.class, false)
			.add(new UniqueFieldValidator<>(MammaMammograaf.class, mammograafId, "aeTitle", hibernateService, true));
		ComponentHelper.addTextField(form, "werkstationIpAdres", true, 15, String.class, false)
			.add(new UniqueFieldValidator<>(MammaMammograaf.class, mammograafId, "werkstationIpAdres", hibernateService, true));

		form.add(new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{

				boolean seProxyEnMammograafIpsMatchen = screeningsEenheidService.ipAdressenHebbenZelfdeGemeenschappelijkeBlokken(getScreeningsEenheid());
				if (!seProxyEnMammograafIpsMatchen)
				{
					error("De eerste drie blokken van de IP-adressen mammograaf en server screeningseenheid komen niet overeen, controleer welke correct is.");
				}
				mammaMammograafService.saveOrUpdateMammograaf(MammaMammograafEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				hibernateService.reload(getScreeningsEenheid());
				opslaan(target);
			}
		});

		form.add(new IndicatingAjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				annuleren(target);
			}

		});
	}

	private MammaScreeningsEenheid getScreeningsEenheid()
	{
		MammaMammograaf mammograaf = MammaMammograafEditPanel.this.getModelObject();
		return mammograaf.getScreeningsEenheid();
	}

	abstract void opslaan(AjaxRequestTarget target);

	abstract void annuleren(AjaxRequestTarget target);
}
