package nl.rivm.screenit.main.web.gebruiker.clienten.complicatie;

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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.ComplicatieErnst;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.model.enums.ComplicatieSoort;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ScopeService;
import nl.rivm.screenit.service.colon.ComplicatieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.poi.ss.formula.functions.T;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ClientComplicatieEditPage extends ClientPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ComplicatieService complicatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private VerslagService verslagService;

	boolean inactiefTonen = Boolean.TRUE;

	private IModel<Complicatie> complicatieModel;

	private IModel<Client> clientModel;

	private ToegangLevel toegangsLevelInzien;

	public ClientComplicatieEditPage(IModel<Client> client, IModel<Complicatie> complicatie)
	{
		super(client);
		complicatieModel = complicatie;
		clientModel = client;
		toegangsLevelInzien = ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN);
		add(new ClientPaspoortPanel("passpoort", client));

		if (ModelUtil.nullSafeGet(complicatieModel) == null)
		{
			inactiefTonen = Boolean.FALSE;
			Complicatie complicatieObject = new Complicatie();
			complicatieObject.setHandmatig(true);
			complicatieModel = ModelUtil.cModel(complicatieObject);
		}

		Form<Complicatie> form = new Form<Complicatie>("form", complicatieModel);
		FormComponent<Date> dateField = ComponentHelper.addTextField(form, "datum", true, 10, Date.class, false);
		dateField.add(DateValidator.maximum(currentDateSupplier.getDate(), "dd-MM-yyyy"));
		dateField.setEnabled(toegangsLevelInzien != null);
		dateField.setLabel(Model.of("Datum"));

		RadioChoice<ComplicatieSoort> soortChoice = new RadioChoice<ComplicatieSoort>("soort", Arrays.asList(ComplicatieSoort.values()),
			new EnumChoiceRenderer<ComplicatieSoort>());
		soortChoice.setPrefix("<label class=\"radio inline\">");
		soortChoice.setSuffix("</label>");
		soortChoice.setRequired(true);
		soortChoice.setLabel(Model.of("Soort"));
		soortChoice.setEnabled(toegangsLevelInzien != null);
		form.add(soortChoice);

		RadioChoice<ComplicatieErnst> ernstChoice = new RadioChoice<ComplicatieErnst>("ernst", Arrays.asList(ComplicatieErnst.values()),
			new EnumChoiceRenderer<ComplicatieErnst>());
		ernstChoice.setPrefix("<label class=\"radio inline\">");
		ernstChoice.setSuffix("</label>");
		ernstChoice.setRequired(true);
		ernstChoice.setLabel(Model.of("Ernst"));
		ernstChoice.setEnabled(toegangsLevelInzien != null);
		form.add(ernstChoice);

		RadioChoice<ComplicatieMoment> momentChoice = new RadioChoice<ComplicatieMoment>("moment", Arrays.asList(ComplicatieMoment.values()),
			new EnumChoiceRenderer<ComplicatieMoment>());
		momentChoice.setPrefix("<label class=\"radio inline\">");
		momentChoice.setSuffix("</label>");
		momentChoice.setLabel(Model.of("Moment"));
		momentChoice.setEnabled(false);
		form.add(momentChoice);

		ScreenitDropdown<MdlVerslag> mdlVerslagen = new ScreenitDropdown<MdlVerslag>("mdlverslag",
			ModelUtil.listRModel(verslagService.getAlleMdlVerslagenVanClient(client.getObject())), new IChoiceRenderer<MdlVerslag>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(MdlVerslag object)
				{
					DateFormat df = new SimpleDateFormat("dd-MM-yyyy");
					String date = "";
					if (object.getDatumOnderzoek() != null)
					{
						date += df.format(object.getDatumOnderzoek());
						date += " - ";
					}
					if (object.getUitvoerderOrganisatie() != null)
					{
						date += object.getUitvoerderOrganisatie().getNaam();
					}
					if (date.equals("") || date.isEmpty())
					{
						date = "MDL verslag binnen ScreenIT";
					}
					return date;
				}

				@Override
				public String getIdValue(MdlVerslag object, int index)
				{
					return object.getId().toString();
				}

				@Override
				public MdlVerslag getObject(String id, IModel<? extends List<? extends MdlVerslag>> choices)
				{
					if (id != null)
					{
						return choices.getObject().stream().filter(o -> o.getId().toString().equals(id)).findFirst().orElse(null);
					}
					return null;
				}
			});
		mdlVerslagen.setNullValid(true);
		mdlVerslagen.setEnabled(toegangsLevelInzien != null);

		form.add(new IndicatingAjaxLink<T>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientComplicatiePage(getClientModel()));
			}
		});

		form.add(new IndicatingAjaxLink<T>("inactief")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Complicatie complicatie = getComplicatieModel().getObject();
				complicatie.setActief(Boolean.FALSE);
				hibernateService.saveOrUpdate(complicatie);
				logService.logGebeurtenis(LogGebeurtenis.COMPLICATIEREGISTER, ScreenitSession.get().getLoggedInInstellingGebruiker(), getClientModel().getObject(),
					"Complicatie verwijderd", Bevolkingsonderzoek.COLON);
				setResponsePage(new ClientComplicatiePage(getClientModel()));
			}

			@Override
			public boolean isVisible()
			{
				ScreenitSession.get().getLoggedInInstellingGebruiker();
				return isInactiefTonen() && ScreenitSession.get().getLoggedInInstellingGebruiker().equals(getComplicatieModel().getObject().getInstellingGebruiker());
			}

		});

		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ColonDossier dossier = getClientModel().getObject().getColonDossier();
				if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getAfspraken().size() > 0)
				{

					InstellingGebruiker account = ScreenitSession.get().getLoggedInInstellingGebruiker();
					Complicatie complicatie = (Complicatie) form.getModelObject();

					Date checkDate = complicatie.getDatum();
					if (complicatie.getMdlverslag() != null && complicatie.getMdlverslag().getDatumOnderzoek() != null)
					{
						checkDate = complicatie.getMdlverslag().getDatumOnderzoek();
					}
					if (complicatieService.magComplicatieVastleggen(checkDate))
					{
						complicatie.setActief(Boolean.TRUE);
						complicatie.setInstellingGebruiker(account);
						complicatie.setClient(getClientModel().getObject());
						complicatie.setMoment(complicatieService.getCorrecteComplicatieMoment(complicatie.getDatum(), complicatie.getMdlverslag()));
						hibernateService.saveOrUpdate(complicatie);
						logService.logGebeurtenis(LogGebeurtenis.COMPLICATIEREGISTER, account, getClientModel().getObject(), "Complicatie ingevoerd of aangepast",
							Bevolkingsonderzoek.COLON);
						setResponsePage(new ClientComplicatiePage(getClientModel()));
					}
					else
					{
						error(getString("error.complicatie.stop.verwerking"));
					}
				}
				else
				{
					error(getString("error.complicatie.statusinvalid"));
				}
			}

			@Override
			public boolean isVisible()
			{
				return toegangsLevelInzien != null;
			}
		});

		form.add(mdlVerslagen);

		add(form);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getClientModel());
		ModelUtil.nullSafeDetach(getComplicatieModel());
	}

	public IModel<Complicatie> getComplicatieModel()
	{
		return complicatieModel;
	}

	public void setComplicatieModel(IModel<Complicatie> complicatieModel)
	{
		this.complicatieModel = complicatieModel;
	}

	@Override
	public IModel<Client> getClientModel()
	{
		return clientModel;
	}

	public void setClientModel(IModel<Client> clientModel)
	{
		this.clientModel = clientModel;
	}

	public boolean isInactiefTonen()
	{
		return inactiefTonen;
	}

	public void setInactiefTonen(boolean inactiefTonen)
	{
		this.inactiefTonen = inactiefTonen;
	}
}
