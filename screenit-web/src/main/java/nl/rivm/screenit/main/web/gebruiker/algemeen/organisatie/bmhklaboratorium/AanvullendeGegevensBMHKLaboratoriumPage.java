
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

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
import java.util.List;
import java.util.Locale;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.pingpong.PingPongInput;
import nl.rivm.screenit.main.web.component.validator.ScreenITIBANValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadInstellingImageFormComponent;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadInstellingImageType;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.GemeenteService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.cervix.CervixBMHKLaboratoriumService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.convert.ConversionException;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.StringValidator;
import org.springframework.dao.DataIntegrityViolationException;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class AanvullendeGegevensBMHKLaboratoriumPage extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private GemeenteService gemeenteService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private CervixBMHKLaboratoriumService cervixBMHKLaboratoriumService;

	public AanvullendeGegevensBMHKLaboratoriumPage()
	{
		Instelling organisatie = getCurrentSelectedOrganisatie();
		Actie actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie, Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER);
		final boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(super.getCurrentSelectedOrganisatie())));

		final IModel<Instelling> model = ModelUtil.cModel(super.getCurrentSelectedOrganisatie());
		setDefaultModel(model);

		Form<Instelling> form = new Form<>("form", model);
		add(form);

		FormComponent<List<String>> instrumentNames = new TextField<List<String>>("instrumentNames")
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings("unchecked")
			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (List.class.isAssignableFrom(type) || ArrayList.class.isAssignableFrom(type))
				{
					return (IConverter<C>) new IConverter<List<String>>()
					{

						private static final long serialVersionUID = 1L;

						@Override
						public List<String> convertToObject(String value, Locale locale)
						{
							List<String> result = new ArrayList<>();
							String[] values = value.split(",");
							for (String splittedValue : values)
							{
								splittedValue = splittedValue.trim();
								if (splittedValue.isEmpty())
								{
									throw new ConversionException("Ongeldige waarde ingevoerd");
								}
								result.add(splittedValue);
							}
							return result;
						}

						@Override
						public String convertToString(List<String> value, Locale locale)
						{
							return StringUtils.join(value, ", ");
						}
					};
				}
				return super.getConverter(type);
			}
		};
		instrumentNames.setEnabled(!inzien);
		form.add(instrumentNames);

		FormComponent<List<String>> userIdScanners = new TextField<List<String>>("userIdScanners")
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings("unchecked")
			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (List.class.isAssignableFrom(type) || ArrayList.class.isAssignableFrom(type))
				{
					return (IConverter<C>) new IConverter<List<String>>()
					{

						private static final long serialVersionUID = 1L;

						@Override
						public List<String> convertToObject(String value, Locale locale)
						{
							List<String> result = new ArrayList<>();
							String[] values = value.split(",");
							for (String splittedValue : values)
							{
								splittedValue = splittedValue.trim();
								if (splittedValue.isEmpty())
								{
									throw new ConversionException("Ongeldige waarde ingevoerd");
								}
								result.add(splittedValue);
							}
							return result;
						}

						@Override
						public String convertToString(List<String> value, Locale locale)
						{
							return StringUtils.join(value, ", ");
						}
					};
				}
				return super.getConverter(type);
			}
		};
		userIdScanners.setEnabled(!inzien);
		form.add(userIdScanners);

		CheckBox oruBerichten = ComponentHelper.newCheckBox("oruBerichtenVerwerken", !inzien);
		form.add(oruBerichten);

		FormComponent<String> ibanField = ComponentHelper.addTextField(form, "iban", true, 34, inzien);
		ibanField.add(new ScreenITIBANValidator());
		ComponentHelper.addTextField(form, "ibanTenaamstelling", true, 70, inzien);

		Component medischMircobioloog = new TextArea<>("medischMircobioloog").add(StringValidator.maximumLength(255));
		medischMircobioloog.setEnabled(!inzien);
		form.add(medischMircobioloog);
		form.add(
			new UploadInstellingImageFormComponent("handtekeningMedischMircobioloog", model, UploadInstellingImageType.BMHK_HANDTEKENING_MEDISCH_MICROBIOLOOG).setEnabled(!inzien));
		Component patholoog = new TextArea<>("patholoog").add(StringValidator.maximumLength(255));
		patholoog.setEnabled(!inzien);
		form.add(patholoog);
		form.add(new UploadInstellingImageFormComponent("handtekeningPatholoog", model, UploadInstellingImageType.BMHK_HANDTEKENING_PATHOLOOG).setEnabled(!inzien));

		ComponentHelper.addTextField(form, "bmhkLabWarnMail", true, 100, inzien).add(EmailAddressValidator.getInstance());

		List<Gemeente> allNietGekoppeldeGemeentes = gemeenteService.getGemeentesZonderBMHKLaboratorium((BMHKLaboratorium) model.getObject());

		SimpleListHibernateModel<Gemeente> choices = new SimpleListHibernateModel<>(allNietGekoppeldeGemeentes);
		ChoiceRenderer<Gemeente> choiceRenderer = new ChoiceRenderer<Gemeente>("naam", "code")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public Object getDisplayValue(Gemeente object)
			{
				Object gemeente = super.getDisplayValue(object);
				if (object.getCode() != null)
				{
					StringBuilder sb = new StringBuilder();
					sb.append(gemeente);
					sb.append(" (");
					sb.append(object.getCode());
					sb.append(")");
					return sb.toString();
				}
				else
				{
					return gemeente;
				}
			}

		};

		final PingPongInput<Gemeente> gemeentes = new PingPongInput<Gemeente>("gemeentes", new PropertyModel<List<Gemeente>>(model, "gemeentes"), choices, choiceRenderer)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Gemeente> model(Gemeente object)
			{
				return ModelUtil.sModel(object);
			}
		};
		gemeentes.setEnabled(!inzien);
		form.add(gemeentes);

		form.add(new AjaxSubmitLink("submit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					cervixBMHKLaboratoriumService.saveOrUpdateLaboratorium((BMHKLaboratorium) model.getObject(), gemeentes.getChoices().getObject());
					BasePage.markeerFormulierenOpgeslagen(target);
					this.info("Gegevens zijn succesvol opgeslagen");
				}
				catch (DataIntegrityViolationException e)
				{
					error(getString("dubbel.id"));
				}
			}

			@Override
			public boolean isVisible()
			{
				return !inzien;
			}

		});

		AjaxLink<Gebruiker> annuleren = new AjaxLink<Gebruiker>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}

			@Override
			public boolean isVisible()
			{
				return !inzien;
			}

		};
		form.add(annuleren);
	}
}
