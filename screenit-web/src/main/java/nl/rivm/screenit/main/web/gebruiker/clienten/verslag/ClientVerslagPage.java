
package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderContext;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.ScreenitFormulierRenderPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsStatement;

public class ClientVerslagPage extends ClientPage
{

	private static final Logger LOG = LoggerFactory.getLogger(ClientVerslagPage.class);

	@SpringBean
	private VerslagService verslagService;

	@SpringBean
	private VerwerkVerslagService verwerkVerslagService;

	@SpringBean
	private FormulierService formulierService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private boolean checkRequired;

	private IModel<FormulierResultaatImpl> formulierResultaatModel;

	private FormulierRenderContext formulierRenderContext;

	private final IModel<? extends Verslag> verslagModel;

	private BootstrapDialog dialog;

	private AjaxSubmitLink opslaanButton;

	private boolean logout = false;

	public ClientVerslagPage(IModel<? extends Verslag> verslagModel)
	{
		super(new PropertyModel<>(verslagModel, "screeningRonde.dossier.client"));
		this.verslagModel = verslagModel;

		Verslag verslag = verslagModel.getObject();

		boolean isOpenstaand = verslag.getStatus().equals(VerslagStatus.IN_BEWERKING);
		boolean isNieuw = verslag.getId() == null;

		boolean magVerslagAanpassen = true;
		boolean magVerslagVerwijderen = false;
		Client client = verslag.getScreeningRonde().getDossier().getClient();

		switch (verslag.getType())
		{
		case MDL:
			magVerslagAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.AANPASSEN, client);
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.VERWIJDEREN, client);
			break;
		case PA_LAB:
			magVerslagAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.AANPASSEN, client);
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.VERWIJDEREN, client);
			break;
		case CERVIX_CYTOLOGIE:
			magVerslagAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_CYTOLOGIE_VERSLAG, Actie.AANPASSEN, client);
			break;
		case MAMMA_PA_FOLLOW_UP:
			magVerslagAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.AANPASSEN, client);
			magVerslagVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.VERWIJDEREN, client);
			break;
		}

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		ScreenitFormulierRenderPanel formulierRenderPanel = new ScreenitFormulierRenderPanel("formulier",
			ModelUtil.sModel(formulierService.getFormulierInstatie(verslag.getType().getTypeFormulier())))
		{
			@Override
			protected boolean checkRequired()
			{
				return checkRequired;
			}

			@Override
			protected boolean isTitleVisible()
			{
				return false;
			}

			@Override
			protected IModel<FormulierResultaatImpl> createRenderContextModel(ScreenitFormulierInstantie formulierInstantie, FormulierRenderContext formulierRenderContext)
			{
				ClientVerslagPage.this.formulierRenderContext = formulierRenderContext;
				Verslag innerVerslag = ClientVerslagPage.this.verslagModel.getObject();
				TypeFormulier typeFormulier = formulierInstantie.getTypeFormulier();
				switch (typeFormulier)
				{
				case MDL:
					formulierRenderContext.setRootObject(new PropertyModel<MdlVerslagContent>(ClientVerslagPage.this.verslagModel, "verslagContent"));
					break;
				case PALGA:
					formulierRenderContext.setRootObject(new PropertyModel<PaVerslagContent>(ClientVerslagPage.this.verslagModel, "verslagContent"));
					break;
				case CYTOLOGIE:
					formulierRenderContext.setRootObject(new PropertyModel<CervixCytologieVerslagContent>(ClientVerslagPage.this.verslagModel, "verslagContent"));
					break;
				case MAMMA_PA_FOLLOW_UP:
					formulierRenderContext.setRootObject(new PropertyModel<MammaFollowUpVerslagContent>(ClientVerslagPage.this.verslagModel, "verslagContent"));
					break;
				default:
					break;

				}

				formulierResultaatModel = super.createRenderContextModel(formulierInstantie, formulierRenderContext);

				verslagService.preFillAntwoorden(innerVerslag, formulierResultaatModel.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker());
				return formulierResultaatModel;
			}

		};
		formulierRenderPanel.setEnabled(isOpenstaand && magVerslagAanpassen);
		add(formulierRenderPanel);

		add(new ClientPaspoortPanel("passpoort", (IModel<Client>) getDefaultModel()));

		Form<Void> form = formulierRenderPanel.getForm();
		form.add(new AbstractFormValidator()
		{
			@Override
			public void validate(Form<?> form)
			{

				Client client = (Client) getDefaultModelObject();
				hibernateService.saveOrUpdate(client);
				client.getComplicaties().size();

				Object rootObject = formulierRenderContext.getRootObject();
				if (rootObject instanceof VerslagContent)
				{
					VerslagContent<?> verslagContent = (VerslagContent<?>) rootObject;
					if (checkRequired)
					{
						FormulierResultaatImpl resultaat = formulierResultaatModel.getObject();

						try
						{
							verwerkVerslagService.valideerVerslagVoorAfronden(verslagContent.getVerslag(), resultaat.getAntwoorden(),
								ScreenitSession.get().getLoggedInInstellingGebruiker());
						}
						catch (IllegalStateException e)
						{
							String message = e.getMessage();
							if (message != null && message.startsWith("error."))
							{
								form.error(getString(message));
							}
						}
					}
				}
			}

			@Override
			public FormComponent<?>[] getDependentFormComponents()
			{
				return null;
			}
		});

		addVerslagVerwijderenButton(isOpenstaand, isNieuw, magVerslagVerwijderen, form);
		addOpslaanButton(isOpenstaand, magVerslagAanpassen, form);
		addAfrondenButton(verslag, isOpenstaand, magVerslagAanpassen, client, form);
		addHeropenenButton(verslag, isOpenstaand, magVerslagAanpassen, client, form);
		addAnnulerenButton();
		addTerugButton();

		add(new WebMarkupContainer("isnieuw").setVisible(isNieuw));

		switch (verslag.getType())
		{
		case MDL:
			add(new Label("type", "MDL"));
			break;
		case PA_LAB:
			add(new Label("type", "PA"));
			break;
		case CERVIX_CYTOLOGIE:
			add(new Label("type", "Cytologie"));
			break;
		case MAMMA_PA_FOLLOW_UP:
			add(new Label("type", "Follow Up"));
			break;
		}
	}

	private void addHeropenenButton(Verslag verslag, boolean isOpenstaand, boolean magVerslagAanpassen, Client client, Form<Void> form)
	{
		IndicatingAjaxLink<Void> heropenen = new IndicatingAjaxLink<Void>("heropenen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{

				Verslag innerVerslag = verslagModel.getObject();
				if (innerVerslag.getType().getBevolkingsonderzoek() != Bevolkingsonderzoek.CERVIX)
				{
					innerVerslag = verslagService.heropenVerslag(innerVerslag, ScreenitSession.get().getLoggedInInstellingGebruiker());
					IModel model = null;
					switch (innerVerslag.getType().getBevolkingsonderzoek())
					{
					case COLON:
						model = ModelUtil.dModel((ColonVerslag<?>) innerVerslag);
						break;
					case MAMMA:
						model = ModelUtil.dModel((MammaVerslag<?>) innerVerslag);
						break;
					default:
						throw new IllegalStateException();
					}
					ScreenitSession.get().info("Verslag heropend");
					setResponsePage(new ClientVerslagPage(model));
					markeerFormulierenOpgeslagen(target);
				}
				else
				{
					ScreenitSession.get().warn("In BMHK is heropenen van verslagen niet toegestaan.");
				}
			}
		};
		boolean magHeropenen = verslag.getStatus().equals(VerslagStatus.AFGEROND) && verslag.getOntvangenBericht() == null && magVerslagAanpassen;
		heropenen.setVisible(magHeropenen);
		add(heropenen);
	}

	@Override
	protected boolean opslaan(AjaxRequestTarget target)
	{
		logout = true;
		JsStatement statement = new JsQuery(opslaanButton).$().chain("click");
		target.appendJavaScript(statement.render());
		return true;
	}

	@Override
	protected String getExtraTimeoutInfo()
	{
		return "Het verslag zal worden opgeslagen mits gegevens geldig zijn.";
	}

	private void addTerugButton()
	{
		add(new Link<Object>("terug")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
			}

		});
	}

	private void addAnnulerenButton()
	{
		add(new Link<Object>("annuleren")
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
			}

		});
	}

	@SuppressWarnings("rawtypes")
	private void addAfrondenButton(Verslag verslag, boolean isOpenstaand, boolean magVerslagAanpassen, Client client, Form<Void> form)
	{
		AjaxSubmitLink afrondenButton = new FormulierIndicatingAjaxSubmitLink("afronden", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				saveOrAfronden(target, true);
			}

			@Override
			protected void onBeforeHandleEvent()
			{
				checkRequired = true;
			}

		};
		boolean magAfronden = verslagService.magAfronden(verslag.getType(), client);
		afrondenButton.setVisible(isOpenstaand && magVerslagAanpassen && magAfronden);
		add(afrondenButton);
	}

	private void addOpslaanButton(boolean isOpenstaand, boolean magVerslagAanpassen, Form<Void> form)
	{
		opslaanButton = new FormulierIndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				saveOrAfronden(target, false);
				if (logout)
				{
					logout();
				}
			}

			@Override
			protected void onBeforeHandleEvent()
			{
				checkRequired = false;
			}

		};
		opslaanButton.setVisible(isOpenstaand && magVerslagAanpassen);
		add(opslaanButton);
	}

	private void addVerslagVerwijderenButton(boolean isOpenstaand, boolean isNieuw, boolean magVerslagVerwijderen, Form<Void> form)
	{
		IndicatingAjaxLink<Void> verwijderen = new ConfirmingIndicatingAjaxLink<Void>("verwijderen", dialog, "verwijder.verslag")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Verslag verslag = verslagModel.getObject();
				if (verslag.getType().getBevolkingsonderzoek() != Bevolkingsonderzoek.CERVIX)
				{
					ScreenitSession.get().info("Verslag verwijderd");
					verslagService.verwijderVerslag(verslag, ScreenitSession.get().getLoggedInInstellingGebruiker());
					markeerFormulierenOpgeslagen(target);

					setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) ClientVerslagPage.this.getDefaultModelObject())));
				}
				else
				{
					ScreenitSession.get().warn("In BMHK is het niet toegestaan om een verslag te verwijderen");
				}
			}

		};
		add(verwijderen);
		verwijderen.setVisible((isOpenstaand || verslagModel.getObject().getOntvangenBericht() != null) && magVerslagVerwijderen && !isNieuw);
	}

	private void saveOrAfronden(AjaxRequestTarget target, boolean afronden)
	{
		Object rootObject = formulierRenderContext.getRootObject();
		if (rootObject instanceof VerslagContent)
		{
			VerslagContent<?> verslagContent = (VerslagContent<?>) rootObject;
			Verslag verslag = verslagContent.getVerslag();
			verslag.setInvoerder(ScreenitSession.get().getLoggedInInstellingGebruiker());
			verslag.setDatumVerwerkt(dateSupplier.getDate());
			verslagService.saveOrAfronden(verslagContent, formulierResultaatModel.getObject(), afronden, ScreenitSession.get().getLoggedInInstellingGebruiker());
		}
		markeerFormulierenOpgeslagen(target);
		setResponsePage(new ClientVerslagenPage(ModelUtil.sModel((Client) getDefaultModelObject())));

	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return ClientVerslagenPage.class;
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return ClientVerslagenPage.class;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return true;
	}

	@Override
	public void detachModels()
	{
		super.detachModels();

		ModelUtil.nullSafeDetach(verslagModel);
		ModelUtil.nullSafeDetach(formulierRenderContext);
	}

}
