package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.validator.CohortJaarValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.dto.UitnodigingCohortDto;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.dto.UitnodigingCohortGeboortejarenDto;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.convert.ConversionException;
import org.apache.wicket.util.convert.IConverter;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	actie = Actie.INZIEN,
	level = ToegangLevel.REGIO,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.COLON,
	constraint = ShiroConstraint.HasPermission)
public class ColonParameterisatiePage extends ParameterisatieBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	public ColonParameterisatiePage()
	{
		Parameterisatie parameteriastie = parameterisatieService.loadParameterisatie();
		add(new CohortenForm("form", new Model<>(parameteriastie)));
		add(new ColonPrimaireParametersPanel("landelijkeParameters", new Model<>(parameteriastie)));
	}

	private class CohortenForm extends Form<Parameterisatie>
	{

		private static final long serialVersionUID = 1L;

		private ToegangLevel level;

		private boolean inzien;

		private IModel<UitnodigingCohortDto> nieuweCohortenModel = new CompoundPropertyModel<>(new Model<>());

		private ListModel<UitnodigingCohortGeboortejarenDto> cohorten;

		public CohortenForm(String id, IModel<Parameterisatie> model)
		{
			super(id, model);
			settingParameterisatieObject(model.getObject());
			final WebMarkupContainer cohortenContainer = new WebMarkupContainer("cohortenContainer");
			cohortenContainer.setOutputMarkupId(true);
			add(cohortenContainer);

			Actie actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
				Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			inzien = !isMinimumActie(actie, Actie.AANPASSEN);

			cohorten = new ListModel<>(nieuweCohortenModel.getObject().getCohorten());

			PropertyListView<UitnodigingCohortGeboortejarenDto> uitnodigingCohorten = new PropertyListView<UitnodigingCohortGeboortejarenDto>("cohorten", cohorten)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<UitnodigingCohortGeboortejarenDto> item)
				{
					FormComponent<List<Integer>> geboorteJaren = new TextField<List<Integer>>("geboortejaren")
					{

						private static final long serialVersionUID = 1L;

						private static final int GELDIGE_SPLITS_WAARDEN = 4;

						@SuppressWarnings("unchecked")
						@Override
						public <C> IConverter<C> getConverter(Class<C> type)
						{
							if (List.class.isAssignableFrom(type))
							{
								return (IConverter<C>) new IConverter<List<Integer>>()
								{

									private static final long serialVersionUID = 1L;

									@Override
									public List<Integer> convertToObject(String value, Locale locale)
									{
										List<Integer> result = new ArrayList<>();
										String[] values = value.split(",");
										for (String splittedValue : values)
										{
											if (!StringUtils.isNumericSpace(splittedValue) || splittedValue.trim().length() != GELDIGE_SPLITS_WAARDEN)
											{
												throw new ConversionException("Ongeldige waarde ingevoerd");
											}

											result.add(Integer.parseInt(splittedValue.trim()));
										}
										return result;
									}

									@Override
									public String convertToString(List<Integer> value, Locale locale)
									{
										Collections.sort(value);
										return StringUtils.join(value, ", ");
									}
								};
							}
							return super.getConverter(type);
						}

					};
					Label jaar = new Label("jaar");
					item.add(jaar);
					geboorteJaren.add(new CohortJaarValidator(item.getModelObject().getJaar()));
					geboorteJaren.setRequired(true);
					geboorteJaren.setEnabled(!inzien);
					item.add(geboorteJaren);
				}
			};
			cohortenContainer.add(uitnodigingCohorten);

			add(new AjaxLink<Parameterisatie>("voegCohortToe", model)
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					UitnodigingCohort uitnodigingCohort = new UitnodigingCohort();
					Integer jaar = Calendar.getInstance().get(Calendar.YEAR);
					for (UitnodigingCohort uitnodigingCohort2 : getModel().getObject().getCohorten())
					{
						if (uitnodigingCohort2.getJaar() >= jaar)
						{
							jaar = uitnodigingCohort2.getJaar() + 1;
						}
					}
					UitnodigingCohortGeboortejarenDto geboortejarenDto = new UitnodigingCohortGeboortejarenDto();
					geboortejarenDto.setJaar(jaar);
					cohorten.getObject().add(geboortejarenDto);
					uitnodigingCohort.setJaar(jaar);
					getModelObject().addCohort(uitnodigingCohort);
					settingParameterisatieObject(getModelObject());
					target.add(cohortenContainer);
				}
			}.setVisible(!inzien && ToegangLevel.LANDELIJK.equals(level)));

			cohortenContainer.add(new AjaxSubmitLink("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					BasePage.markeerFormulierenOpgeslagen(target);
					nieuweCohortenModel.getObject().setCohorten(cohorten.getObject());
					parameterisatieService.saveParametrisatieCohort(ScreenitSession.get().getLoggedInAccount(), getModelObject().getCohorten(), nieuweCohortenModel.getObject());
					markeerFormulierenOpgeslagen(target);
					info("Cohorten zijn opgeslagen");
				}
			}.setVisible(!inzien && ToegangLevel.LANDELIJK.equals(level)));
		}

		private void settingParameterisatieObject(Parameterisatie parameterisatieObject)
		{
			UitnodigingCohortDto uitnodigingCohortDto = new UitnodigingCohortDto();
			List<UitnodigingCohortGeboortejarenDto> geboortejarenDtos = new ArrayList<>();

			for (UitnodigingCohort cohort : parameterisatieObject.getCohorten())
			{
				UitnodigingCohortGeboortejarenDto geboortejarenDto = new UitnodigingCohortGeboortejarenDto();
				geboortejarenDto.setJaar(cohort.getJaar());
				geboortejarenDto.setGeboortejaren(cohort.getGeboortejaren().stream().map(UitnodigingCohortGeboortejaren::getGeboortejaren).collect(Collectors.toList()));
				geboortejarenDtos.add(geboortejarenDto);
			}
			uitnodigingCohortDto.setCohorten(geboortejarenDtos);
			nieuweCohortenModel.setObject(uitnodigingCohortDto);
		}

		@Override
		protected void onDetach()
		{
			super.onDetach();
			ModelUtil.nullSafeDetach(cohorten);
			ModelUtil.nullSafeDetach(nieuweCohortenModel);
		}
	}
}
