package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.cervix.facturatie.CervixBetalingsZoekObject;
import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.HibernateIdChoiceRenderer;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.cervix.CervixHerindexeringWaarschuwingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.DistributedLockService;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX },
	recht = { Recht.GEBRUIKER_SCREENING_BETALINGEN_BMHK },
	organisatieTypeScopes = { OrganisatieType.RIVM },
	checkScope = true)
public class CervixBetalingPage extends CervixScreeningBasePage
{

	@SpringBean
	private CervixBetalingService betalingService;

	@SpringBean
	private DistributedLockService lockService;

	@SpringBean
	private InstellingService instellingService;

	private final IModel<CervixBetalingsZoekObject> zoekObjectModel;

	public CervixBetalingPage()
	{
		CervixBetalingsZoekObject zoekObject = new CervixBetalingsZoekObject();
		zoekObject.setVerrichtingenHuisarts(true);
		zoekObject.setVerrichtingenLaboratorium(true);
		zoekObjectModel = new CompoundPropertyModel<>(zoekObject);

		add(getFilterForm());
	}

	private ScreenitForm<CervixBetalingsZoekObject> getFilterForm()
	{
		ScreenitForm<CervixBetalingsZoekObject> form = new ScreenitForm<>("form", zoekObjectModel);

		form.add(new CervixHerindexeringWaarschuwingPanel("waarschuwing"));

		form.add(ComponentHelper.newDatePicker("verrichtingsdatumTotEnMet"));

		CheckBox verrichtingenLaboratoriumCheckbox = ComponentHelper.newCheckBox("verrichtingenLaboratorium");
		form.add(verrichtingenLaboratoriumCheckbox);
		CheckBox verrichtingenHuisartsCheckbox = ComponentHelper.newCheckBox("verrichtingenHuisarts");
		form.add(verrichtingenHuisartsCheckbox);

		var betalenButton = new IndicatingAjaxSubmitLink("betalen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				navigeerNaarBetalingOverzichtPaginaIndienNiemandAndersBezigIs();
			}
		};

		betalenButton.setVisible(false);
		betalenButton.setOutputMarkupPlaceholderTag(true);

		form.add(betalenButton);

		var teKiezenScreeningOrganisaties = instellingService.getAllActiefScreeningOrganisaties();

		var screeningOrganisatieDropdown = new ScreenitDropdown<>("screeningOrganisatieId",
			teKiezenScreeningOrganisaties.stream()
				.map(Instelling::getId)
				.collect(Collectors.toList()),
			new HibernateIdChoiceRenderer(teKiezenScreeningOrganisaties, "naam"));

		screeningOrganisatieDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				betalenButton.setVisible(zoekObjectModel.getObject().getScreeningOrganisatieId() != null);
				target.add(betalenButton);
			}
		});

		form.add(screeningOrganisatieDropdown);

		form.add(new AbstractFormValidator()
		{
			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return new FormComponent[] { verrichtingenLaboratoriumCheckbox, verrichtingenHuisartsCheckbox };
			}

			@Override
			public void validate(Form<?> form)
			{
				Boolean toonVerrichtingenLaboratorium = verrichtingenLaboratoriumCheckbox.getConvertedInput();
				Boolean toonVerrichtingenHuisarts = verrichtingenHuisartsCheckbox.getConvertedInput();
				if (BooleanUtils.isNotTrue(toonVerrichtingenLaboratorium) && BooleanUtils.isNotTrue(toonVerrichtingenHuisarts))
				{
					form.error("Er zijn geen verrichtingen om te betalen, kies huisartsverrichtingen en/of laboratoriumverrichtingen.");
				}
			}
		});

		return form;
	}

	private void navigeerNaarBetalingOverzichtPaginaIndienNiemandAndersBezigIs()
	{
		if (lockService.verkrijgLockIndienBeschikbaar(Constants.BMHK_BETALING_GENEREREN_LOCKNAAM))
		{
			try
			{
				var boekregels = betalingService.getVerrichtingenVoorBetaling(zoekObjectModel.getObject());
				if (boekregels.isEmpty())
				{
					info("Er zijn op dit moment geen verrichtingen meer die uitbetaald moeten worden.");
				}
				else
				{
					setResponsePage(new CervixBetalingOverzichtPage(boekregels, zoekObjectModel.getObject().getScreeningOrganisatieId()));

				}
			}
			finally
			{
				lockService.unlock(Constants.BMHK_BETALING_GENEREREN_LOCKNAAM);
			}
		}
		else
		{
			info(getString("info.bmhk.overzicht.genereren.al.in.gebruik"));
		}
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen", CervixBetalingPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen.sepabestanden", CervixBetalingSepaBestandenPage.class));
		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
