package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.price.BigDecimalPriceLabel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX },
	recht = { Recht.GEBRUIKER_SCREENING_BETALINGEN_BMHK },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE })
public class CervixBetalingOverzichtPage extends CervixScreeningBasePage
{
	@SpringBean
	private CervixBetalingService cervixBetalingService;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	@SpringBean
	private HibernateService hibernateService;

	private IModel<CervixBetaalopdracht> betaalopdrachtModel;

	private IModel<List<CervixBetaalopdrachtRegel>> laboratoriumBetaalopdrachtRegels;

	private IModel<List<CervixBetaalopdrachtRegelSpecificatie>> laboratoriumBetaalRegelSpecificaties;

	private IModel<List<CervixBetaalopdrachtRegelSpecificatie>> huisartsenBetaalRegelSpecificaties;

	public CervixBetalingOverzichtPage(List<CervixBoekRegel> boekregels)
	{
		ScreeningOrganisatie organisatie = ScreenitSession.get().getScreeningOrganisatie();
		betaalopdrachtModel = ModelUtil
			.cModel(cervixVerrichtingService.createBetaalOpdracht(organisatie, boekregels));

		Form<CervixBetaalopdracht> form = new Form<CervixBetaalopdracht>("form", betaalopdrachtModel);
		add(form);

		laboratoriumBetaalopdrachtRegels = getAlleLaboratoriumBetaalRegels(betaalopdrachtModel.getObject());

		ComponentHelper.addTextField(form, "omschrijving", true, 255, false);

		ListView<CervixBetaalopdrachtRegel> laboratoriumListView = new ListView<CervixBetaalopdrachtRegel>("labs",
			laboratoriumBetaalopdrachtRegels)
		{
			@Override
			protected void populateItem(ListItem<CervixBetaalopdrachtRegel> listItem)
			{
				CervixBetaalopdrachtRegel regel = listItem.getModelObject();
				BMHKLaboratorium laboratorium = regel.getLaboratorium();
				listItem.add(new Label("naam", Model.of(laboratorium.getNaam() + " - " + laboratorium.getIban().toUpperCase())));

				laboratoriumBetaalRegelSpecificaties = getBetaalRegelSpecificatiesVoorLab(laboratorium, betaalopdrachtModel.getObject());

				ListView<CervixBetaalopdrachtRegelSpecificatie> listOpdrachtRegels = new ListView<CervixBetaalopdrachtRegelSpecificatie>("betaalopdrachtregels",
					laboratoriumBetaalRegelSpecificaties)
				{

					@Override
					protected void populateItem(ListItem<CervixBetaalopdrachtRegelSpecificatie> listItem)
					{
						CervixBetaalopdrachtRegelSpecificatie spec = listItem.getModelObject();
						CervixBetaalopdrachtRegel opdrachtRegel = spec.getBetaalopdrachtRegel();
						listItem.add(new Label("tenaamstelling", Model.of(opdrachtRegel.getNaarTenaamstelling())));
						listItem.add(new Label("naarIban", Model.of(opdrachtRegel.getNaarIban())));
						listItem.add(new EnumLabel<>("type", spec.getTariefType()));
						listItem.add(new Label("aantal", Model.of(spec.getAantalBetaalRegels())));
						listItem.add(new BigDecimalPriceLabel("tarief", spec.getTariefBedrag()));
						listItem.add(new BigDecimalPriceLabel("debets", spec.getDebets()));
						listItem.add(new BigDecimalPriceLabel("subtotaalZonderDebets", spec.getBedrag().add(spec.getDebets())));
						listItem.add(new BigDecimalPriceLabel("subtotaal", spec.getBedrag()));
					}
				};
				listItem.add(listOpdrachtRegels);
				listItem.add(new BigDecimalPriceLabel("totaal", regel.getBedrag()));
			}
		};
		form.add(laboratoriumListView);

		form.add(getHuisartsenContainer());

		form.add(new AjaxLink<Void>("terug")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(CervixBetalingPage.class);
			}
		});

		form.add(new IndicatingAjaxSubmitLink("genereren", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixBetaalopdracht opdracht = (CervixBetaalopdracht) form.getModelObject();
				cervixBetalingService.archiveerBestaandeOpdrachten(opdracht.getScreeningOrganisatie());
				Long opdrachtId = cervixBetalingService.opslaanBetaalopdracht(opdracht);
				cervixBetalingService.genereerCervixBetalingsSpecificatieEnSepaBestand(opdrachtId);
				setResponsePage(CervixBetalingSepaBestandenPage.class);
				ScreenitSession.get().info(getString("sepa.bestand.genereren"));
			}
		});
	}

	private IModel<List<CervixBetaalopdrachtRegelSpecificatie>> getBetaalRegelSpecificatiesVoorLab(BMHKLaboratorium bmhkLaboratorium, CervixBetaalopdracht opdracht)
	{
		List<CervixBetaalopdrachtRegelSpecificatie> regels = opdracht.getBetaalopdrachtRegels().stream()
			.filter(b -> b.getLaboratorium() != null && b.getLaboratorium().equals(bmhkLaboratorium))
			.map(CervixBetaalopdrachtRegel::getSpecificaties).flatMap(Collection::stream)
			.collect(Collectors.toList());
		return ModelUtil.listModel(regels);
	}

	private IModel<List<CervixBetaalopdrachtRegel>> getAlleLaboratoriumBetaalRegels(CervixBetaalopdracht opdracht)
	{
		List<CervixBetaalopdrachtRegel> regels = opdracht.getBetaalopdrachtRegels().stream()
			.filter(b -> b.getLaboratorium() != null)
			.sorted((o1, o2) -> o1.getNaarTenaamstelling().compareTo(o2.getNaarTenaamstelling()))
			.collect(Collectors.toList());
		return ModelUtil.listModel(regels);
	}

	private IModel<List<CervixBetaalopdrachtRegelSpecificatie>> getHuisartsBetaalregelSpecificaties(CervixBetaalopdracht opdracht)
	{
		List<CervixBetaalopdrachtRegelSpecificatie> regels = opdracht.getBetaalopdrachtRegels().stream()
			.filter(b -> b.getHuisartsLocatie() != null)
			.sorted((o1, o2) -> o1.getNaarTenaamstelling().compareTo(o2.getNaarTenaamstelling()))
			.map(CervixBetaalopdrachtRegel::getSpecificaties).flatMap(Collection::stream)
			.collect(Collectors.toList());
		return ModelUtil.listModel(regels);
	}

	private WebMarkupContainer getHuisartsenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("huisartsenContainer");
		huisartsenBetaalRegelSpecificaties = getHuisartsBetaalregelSpecificaties(betaalopdrachtModel.getObject());
		boolean zijnErHuisartsGegevens = huisartsenBetaalRegelSpecificaties.getObject().size() > 0;

		ListView<CervixBetaalopdrachtRegelSpecificatie> huisartsenRegelsListView = new ListView<CervixBetaalopdrachtRegelSpecificatie>("ḧuisartsRegels",
			huisartsenBetaalRegelSpecificaties)
		{
			@Override
			protected void populateItem(ListItem<CervixBetaalopdrachtRegelSpecificatie> listItem)
			{
				CervixBetaalopdrachtRegelSpecificatie spec = listItem.getModelObject();
				CervixBetaalopdrachtRegel regel = spec.getBetaalopdrachtRegel();
				listItem.add(new Label("tenaamstelling", Model.of(regel.getNaarTenaamstelling())));
				listItem.add(new Label("naarIban", Model.of(regel.getNaarIban())));
				listItem.add(new EnumLabel<>("type", spec.getTariefType()));
				listItem.add(new Label("aantal", Model.of(spec.getAantalBetaalRegels())));
				listItem.add(new BigDecimalPriceLabel("tarief", spec.getTariefBedrag()));
				listItem.add(new BigDecimalPriceLabel("debets", spec.getDebets()));
				listItem.add(new BigDecimalPriceLabel("subtotaalZonderDebets", spec.getBedrag().add(spec.getDebets())));
				listItem.add(new BigDecimalPriceLabel("subtotaal", spec.getBedrag()));
			}
		};
		container.add(huisartsenRegelsListView);

		container.setVisible(zijnErHuisartsGegevens);
		return container;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen", CervixBetalingPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen.controle", false, CervixBetalingOverzichtPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.cervixscreening.betalingen.sepabestanden", CervixBetalingSepaBestandenPage.class));
		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(betaalopdrachtModel);
		ModelUtil.nullSafeDetach(laboratoriumBetaalopdrachtRegels);
		ModelUtil.nullSafeDetach(laboratoriumBetaalRegelSpecificaties);
		ModelUtil.nullSafeDetach(huisartsenBetaalRegelSpecificaties);
	}
}
