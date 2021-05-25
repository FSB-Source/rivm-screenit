package nl.rivm.screenit.main.web.gebruiker.screening.cervix.huisarts;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.AchternaamValidator;
import nl.rivm.screenit.main.web.component.validator.TussenvoegselValidator;
import nl.rivm.screenit.main.web.component.validator.VoorlettersValidator;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.service.GemeenteService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.RangeValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class CervixKlaarzettenHuisartsPanel extends GenericPanel<CervixHuisarts>
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(CervixKlaarzettenHuisartsPanel.class);

	@SpringBean
	private CervixHuisartsService cervixHuisartsService;

	@SpringBean
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@SpringBean
	private GemeenteService gemeenteService;

	@SpringBean
	private HibernateService hibernateService;

	private Form<CervixHuisarts> form;

	private IModel<List<Woonplaats>> alleWoonplaatsen;

	public CervixKlaarzettenHuisartsPanel(String id, CervixHuisarts arts)
	{
		super(id, ModelUtil.cModel(arts));

		alleWoonplaatsen = ModelUtil.listRModel(hibernateService.loadAll(Woonplaats.class, "naam", true));

		form = new ScreenitForm<>("form", getModel());
		add(form);

		form.add(new Label("agbcode"));

		String laatsteBrief = "";
		if (arts.getId() != null)
		{
			CervixRegioBrief brief = cervixHuisartsService.getLaatsteRegistratieBrief(arts);
			if (brief != null)
			{
				laatsteBrief = "; laatste uitnodiging klaargezet op " + Constants.getDateFormat().format(brief.getCreatieDatum());
			}
		}
		boolean uitEnovation = !CollectionUtils.isEmpty(arts.getHuisartsLocaties());
		form.add(new Label("agbStatus", arts.getId() != null ? "AGB is al geregistreerd" + laatsteBrief : uitEnovation ? "Informatie overgenomen van E-novation lijst" : "Nieuw"));
		ComponentHelper.addTextField(form, "email", true, 100, String.class, false).add(EmailAddressValidator.getInstance());
		ComponentHelper.addTextField(form, "postadres.straat", true, 43, String.class, false);
		ComponentHelper.addTextField(form, "postadres.huisnummer", true, 10, Integer.class, false).add(RangeValidator.minimum(0));
		ComponentHelper.addTextField(form, "postadres.huisnummerToevoeging", false, 26, false);
		ComponentHelper.newPostcodeTextField(form, "postadres.postcode", true, false);
		form.add(new ScreenitDropdown<Woonplaats>("postadres.woonplaats", alleWoonplaatsen, new IChoiceRenderer<Woonplaats>()
		{
			@Override
			public Object getDisplayValue(Woonplaats object)
			{

				return object.getNaam() + " (Gemeente: " + object.getGemeente().getNaam() + ")";
			}

			@Override
			public String getIdValue(Woonplaats object, int index)
			{
				return object.getId().toString();
			}

			@Override
			public Woonplaats getObject(String id, IModel<? extends List<? extends Woonplaats>> choices)
			{
				if (id != null)
				{
					return choices.getObject().stream().filter(o -> o.getId().toString().equals(id)).findFirst().orElse(null);
				}
				return null;
			}
		}).setRequired(true));

		ComponentHelper.addDropDownChoiceINaam(form, "organisatieMedewerkers[0].medewerker.aanhef", false, Arrays.asList(Aanhef.values()), false).setNullValid(true);

		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.achternaam", true, 50, false).add(new AchternaamValidator());
		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.voorletters", false, 20, false).add(new VoorlettersValidator());
		ComponentHelper.addTextField(form, "organisatieMedewerkers[0].medewerker.tussenvoegsel", false, 20, false).add(new TussenvoegselValidator());

		form.add(new IndicatingAjaxSubmitLink("uitnodigingVersturen", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixHuisarts arts = getModelObject();
				String woonplaatsControleMelding = controleerWoonplaats(arts);
				if (StringUtils.isBlank(woonplaatsControleMelding))
				{

					arts = cervixHuisartsService.maakOfWijzigUitstrijkendArts(arts, ScreenitSession.get().getLoggedInInstellingGebruiker());
					cervixHuisartsService.sendRegistratieMail(arts);
					cervixHuisartsSyncService.sendData(arts);
					if (!CollectionUtils.isEmpty(arts.getHuisartsLocaties()))
					{
						cervixHuisartsService.saveCervixHuisartsLocatie(arts.getHuisartsLocaties());
					}

					ScreenitSession.get().info("Uitnodiging klaargezet.");
					closeAanvraag(target);
				}
				else
				{
					LOG.error(woonplaatsControleMelding);
					ScreenitSession.get().error(woonplaatsControleMelding);
				}
			}
		});

		form.add(new IndicatingAjaxLink<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				closeAanvraag(target);
			}
		});
	}

	private String controleerWoonplaats(CervixHuisarts huisarts)
	{
		Woonplaats woonplaats = huisarts.getPostadres().getWoonplaats();
		if (woonplaats.getGemeente() == null)
		{
			return "Er is geen gemeente gekoppeld aan woonplaats: " + woonplaats.getNaam() + ". Neem contact op met de helpdesk.";
		}
		if (woonplaats.getGemeente().getScreeningOrganisatie() == null)
		{
			return "Er is geen screeningsorganisatie gekoppeld aan de gemeente " + woonplaats.getGemeente().getNaam() + ". Neem contact op met de helpdesk.";
		}
		return null;
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(OnDomReadyHeaderItem.forScript("cervixUAFindOutAdressesEquals();"));
	}

	protected abstract void closeAanvraag(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(alleWoonplaatsen);
	}
}
