package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline;

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

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;

public class MammaTestSluitDagenSEPanel extends Panel
{
	@SpringBean
	private MammaTestTimelineService testTimelineService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private IModel<MammaScreeningsEenheid> screeningsEenheid;

	public MammaTestSluitDagenSEPanel(String id)
	{
		super(id);

		var form = new ScreenitForm<>("sluitDagenSEForm");
		add(form);
		form.add(maakScreeningsEenheidDropdown());
		form.add(maakSluitDagenKnop(form));
	}

	@NotNull
	private ScreenitDropdown<MammaScreeningsEenheid> maakScreeningsEenheidDropdown()
	{
		var mogelijkeOpties = ModelUtil.listRModel(screeningsEenheidService.getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreenitSession.get().getScreeningOrganisatie()));

		var dropdown = ComponentHelper.newDropDownChoice("screeningsEenheidDropdown", mogelijkeOpties, new ChoiceRenderer<>("naam"), true);
		dropdown.setModel(new PropertyModel<>(this, "screeningsEenheid"));

		return dropdown;
	}

	@NotNull
	private IndicatingAjaxButton maakSluitDagenKnop(ScreenitForm<Object> form)
	{
		return new IndicatingAjaxButton("sluitDagen", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
				try
				{
					testTimelineService.sluitAlleDagenTotEnMetGisteren(screeningsEenheid.getObject(), ingelogdeGebruiker);
					info("Dagen afgesloten");
				}
				catch (IllegalStateException ex)
				{
					error(ex.getMessage());
				}
			}
		};
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return ModelUtil.nullSafeGet(screeningsEenheid);
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = ModelUtil.sModel(screeningsEenheid);
	}
}
