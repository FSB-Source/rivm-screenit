
package nl.rivm.screenit.main.web.gebruiker.base;

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

import nl.rivm.screenit.main.model.mamma.MammaImsUserSessionType;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.panels.ApplicatieInfoPanel;
import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerBasisgegevens;
import nl.rivm.screenit.main.web.gebruiker.dashboard.DashboardPage;
import nl.rivm.screenit.main.web.gebruiker.login.uzipas.zorgid.session.SessionCheckingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.security.IScreenitRealm;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingReserveringService;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.CallbackParameter;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBoxMultipleChoice;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.request.IRequestParameters;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.StringValue;
import org.wicketstuff.wiquery.core.javascript.JsStatement;

public abstract class GebruikerBasePage extends BasePage
{
	private static final String FADE_ALERT_SUCCES_SCRIPT = "fadeAlertSucces()";

	public static final String IMS_DESKTOPSYNC_JS_SOURCE = "assets/js/imsintegratie/ims-desktopsync.js";

	public static final String IMS_KEYPAD_JS_SOURCE = "assets/js/imsintegratie/ims-keypad.js";

	private static final String MAMMA_IMS_ERROR_CALLBACK_PARAM_MESSAGE = "ERROR_MESSAGE";

	private static final String MAMMA_IMS_ERROR_CALLBACK_PARAM_ONDERZOEK = "ONDERZOEK_ID";

	private AbstractDefaultAjaxBehavior imsErrorCallbackBehavior;

	private Panel feedbackPanel;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private DashboardService dashboardService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaImsService imsService;

	@SpringBean
	private IScreenitRealm realm;

	private AbstractDefaultAjaxBehavior keepAliveBehavior;

	private AbstractDefaultAjaxBehavior logoutBehavior;

	protected BootstrapDialog dialog;

	private boolean loginIms = false;

	private final boolean heeftImsKoppelingRecht;

	private final boolean isMammaBeoordelaar;

	@SpringBean
	protected MammaBaseBeoordelingReserveringService beoordelingReserveringService;

	public GebruikerBasePage()
	{
		ScreenitSession.get().setInPlanningmodule(false);
		heeftImsKoppelingRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN);
		isMammaBeoordelaar = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST, Actie.AANPASSEN) &&
			OrganisatieType.BEOORDELINGSEENHEID.equals(ScreenitSession.get().getInstelling().getOrganisatieType());

		if ((isMammaBeoordelaar || heeftImsKoppelingRecht) && !(this instanceof AbstractMammaBeoordelenPage))
		{
			beoordelingReserveringService.reserveringenVrijgeven(ScreenitSession.get().getLoggedInInstellingGebruiker());
		}
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new ApplicatieInfoPanel("applicatieInfo"));
		addMenu();
		addBvoFilter();

		dialog = new BootstrapDialog("baseDialog");
		add(dialog);

		feedbackPanel = new BootstrapFeedbackPanel("feedback");
		add(feedbackPanel);

		add(new SessionCheckingPanel("zorgIdChecking")
		{
			@Override
			protected void logout()
			{
				GebruikerBasePage.this.logout();
			}
		});

		TransparentWebMarkupContainer pageLargecontainer = new TransparentWebMarkupContainer("pageLarge");
		if (getPageLarge())
		{
			pageLargecontainer.add(new AttributeAppender("class", Model.of("page-large"), " "));
		}

		add(pageLargecontainer);
		add(maakContextMenuExtensie("tabExtensie"));
		add(new ContextMenuPanel("contextMenu")
		{
			@Override
			protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
			{
				return GebruikerBasePage.this.getActiveContextMenuClass();
			}

			@Override
			protected List<GebruikerMenuItem> getAllowedContextMenuItems()
			{
				return GebruikerBasePage.this.getAllowedContextMenuItems();
			}
		});

		WebMarkupContainer naarMedewerkerGegevens = new WebMarkupContainer("naarMedewerkerGegevens");
		InstellingGebruiker ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		if (ingelogdeGebruiker != null)
		{
			if (autorisatieService.getActieVoorMedewerker(ingelogdeGebruiker, null, Recht.GEBRUIKER_MEDEWERKER_BEHEER) != null)
			{
				naarMedewerkerGegevens = new AjaxLink<Object>("naarMedewerkerGegevens")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						setResponsePage(new MedewerkerBasisgegevens(ModelUtil.cModel(ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker())));
					}
				};
			}
			naarMedewerkerGegevens.add(new Label("ingelogdeGebruiker", NaamUtil.getNaamGebruiker(ingelogdeGebruiker.getMedewerker())));
		}
		else
		{
			naarMedewerkerGegevens.add(new Label("ingelogdeGebruiker"));
		}

		add(naarMedewerkerGegevens);
		add(new OrganisatieWisselPanel("organisatiewissel"));

		afmeldenConformAjaxDialog();

		keepAliveBehavior = new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{

			}
		};
		add(keepAliveBehavior);

		logoutBehavior = new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				if (!opslaan(target))
				{
					logout();
				}
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);

				if (isHeeftImsKoppelingRecht())
				{
					logoutFromIms(attributes);
				}
			}
		};
		add(logoutBehavior);

		String extaTimeoutInfo = getExtraTimeoutInfo();
		add(new Label("extraTimeoutInfo", extaTimeoutInfo).setVisible(StringUtils.isNotBlank(extaTimeoutInfo)));

		if (isHeeftImsKoppelingRecht())
		{
			createImsErrorCallback();
		}
	}

	protected boolean getPageLarge()
	{
		return false;
	}

	protected Component maakContextMenuExtensie(String id)
	{
		return new EmptyPanel(id).setVisible(false);
	}

	protected boolean opslaan(AjaxRequestTarget target)
	{
		return false; 
	}

	protected void logout()
	{
		if (isMammaBeoordelaar || heeftImsKoppelingRecht) 
		{
			beoordelingReserveringService.reserveringenVrijgeven(ScreenitSession.get().getLoggedInInstellingGebruiker());
		}
		Class<? extends Page> homePage = Application.get().getHomePage();
		ScreenitSession.get().logout();
		setResponsePage(homePage);
	}

	protected String getExtraTimeoutInfo()
	{
		return null;
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);

		response.render(JavaScriptHeaderItem.forUrl("assets/js/libs/daypilot/daypilot-all.min.js"));

		setupTimeout(response);

		if (heeftImsKoppelingRecht)
		{
			response.render(JavaScriptHeaderItem.forScript(createImsErrorAfhandeling(), null));
			response.render(JavaScriptHeaderItem.forUrl(IMS_DESKTOPSYNC_JS_SOURCE));
			response.render(JavaScriptHeaderItem.forUrl(IMS_KEYPAD_JS_SOURCE));
			if (loginIms)
			{
				response.render(JavaScriptHeaderItem.forScript(createImsLogonCommand(), null));
			}
		}
	}

	protected void setupTimeout(IHeaderResponse response)
	{
		response.render(JavaScriptHeaderItem.forReference(TimeoutResponseJsResourceReference.get()));
		response.render(JavaScriptHeaderItem.forReference(TimeoutJsResourceReference.get()));

		int timeoutMillis = ((ServletWebRequest) RequestCycle.get().getRequest()).getContainerRequest().getSession().getMaxInactiveInterval() * 1000;

		int meldingTimeoutMillis = timeoutMillis - (5 * 60 + 10) * 1000;

		JsStatement jsStatement = new JsStatement();
		jsStatement.append("screenit.initClientResponse();");
		jsStatement.append("screenit.meldingTimeout=" + meldingTimeoutMillis + ";");
		jsStatement.append("screenit.keepAliveCallback=" + keepAliveBehavior.getCallbackFunction() + ";");
		jsStatement.append("screenit.logoutCallback=" + logoutBehavior.getCallbackFunction() + ";");
		jsStatement.append("screenit.startSessionTimer();");
		jsStatement.append(FADE_ALERT_SUCCES_SCRIPT);

		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}

	private String createImsLogonCommand()
	{
		String imsLogonMessage = imsService.createLogonMessage(ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker(),
			ScreenitSession.get().getMammaHuidigeIDS7Role());
		return createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType.LogOn, imsLogonMessage);
	}

	protected String createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType messageType, String message)
	{
		return createUserSessionToImsBridgeSendCommand(messageType, message, null);
	}

	protected String createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType messageType, String message, Long onderzoekIdVoorErrorCallback)
	{
		return String.format("sendUserSessionToImsBridge('%s',%s, %s);", messageType, message, onderzoekIdVoorErrorCallback);
	}

	private String createImsLogoffCommand()
	{
		String imsLogoffMessage = imsService.createLogoffMessage(ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker(),
			ScreenitSession.get().getMammaHuidigeIDS7Role());
		return createUserSessionToImsBridgeSendCommand(MammaImsUserSessionType.LogOff, imsLogoffMessage) + "window.imsLogOffSent = true;";
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{
		target.add(feedbackPanel);
		target.appendJavaScript(FADE_ALERT_SUCCES_SCRIPT);
	}

	private void addBvoFilter()
	{
		WebMarkupContainer container = new WebMarkupContainer("bvoFilterContainer");
		add(container);

		InstellingGebruiker gebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		List<Bevolkingsonderzoek> onderzoeken = Bevolkingsonderzoek.sort(autorisatieService.getBevolkingsonderzoeken(gebruiker));
		if (onderzoeken.size() < 2)
		{
			container.setVisible(false);
			container.add(new EmptyPanel("bvoForm"));
			if (onderzoeken.size() == 1 && !gebruiker.getBevolkingsonderzoeken().contains(onderzoeken.get(0)))
			{
				ScreenitSession.get().getOnderzoeken().add(onderzoeken.get(0));
				gebruiker.setBevolkingsonderzoeken(onderzoeken);
				hibernateService.saveOrUpdate(gebruiker);
				realm.clearCachedAuthorizationInfo(gebruiker);
			}
			return;
		}
		container.setVisible(true);
		Form<InstellingGebruiker> bvoForm = new Form<>("bvoForm", ModelUtil.cModel(gebruiker));
		CheckBoxMultipleChoice<Bevolkingsonderzoek> keuzemaken = new CheckBoxMultipleChoice<>("bevolkingsonderzoeken", onderzoeken,
			new ChoiceRenderer<Bevolkingsonderzoek>()
			{
				@Override
				public Object getDisplayValue(Bevolkingsonderzoek object)
				{
					return " " + object.getAfkorting();
				}
			});
		keuzemaken.add(new AjaxFormSubmitBehavior(bvoForm, "change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				InstellingGebruiker instGebruiker = (InstellingGebruiker) getForm().getDefaultModelObject();
				hibernateService.saveOrUpdate(instGebruiker);
				realm.clearCachedAuthorizationInfo(instGebruiker);
				setResponsePage(ScreenitSession.get().getPageForInstellingGebruiker(instGebruiker));
			}
		});
		keuzemaken.setSuffix("<br>");
		bvoForm.add(keuzemaken);
		container.add(bvoForm);
	}

	private void addMenu()
	{
		ListView<GebruikerMenuItem> hoofdmenu = new ListView<GebruikerMenuItem>("hoofdMenu", getHoofdMenuItems())
		{
			@Override
			protected void populateItem(ListItem<GebruikerMenuItem> item)
			{
				WebMarkupContainer container = new WebMarkupContainer("container");
				Panel contextMenu;
				WebMarkupContainer link;
				WebMarkupContainer caret = new WebMarkupContainer("caret");

				WebMarkupContainer status = new WebMarkupContainer("statusDashboard");
				status.setVisible(false);
				if (DashboardPage.class.equals(item.getModelObject().getTargetPageClass()))
				{
					BvoZoekCriteria dZC = new BvoZoekCriteria();
					dZC.setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
					final Level level = dashboardService.getHoogsteLevelDashboardItems(ScreenitSession.get().getInstelling(), ScreenitSession.get().getOnderzoeken());
					status = new WebMarkupContainer("statusDashboard")
					{
						@Override
						protected void onComponentTag(ComponentTag tag)
						{
							super.onComponentTag(tag);
							if (Level.ERROR.equals(level))
							{
								tag.put("class", "dashboard-status red");
							}
							else if (Level.WARNING.equals(level))
							{
								tag.put("class", "dashboard-status orange");
							}
						}

					};
					status.setVisible(!Level.INFO.equals(level));
				}

				final GebruikerMenuItem gebruikerMenuItem = item.getModelObject();
				if (gebruikerMenuItem.equals(getActieveMenuItem().getMenuItem()))
				{
					container.add(new AttributeAppender("class", new Model<>("active"), " "));
				}

				if (CollectionUtils.isNotEmpty(gebruikerMenuItem.getSubMenuItems()))
				{
					container.add(new AttributeAppender("class", new Model<>("dropdown"), " "));
					link = new WebMarkupContainer("link");
					link.add(new AttributeAppender("data-toggle", new Model<>("dropdown"), " "));
					link.add(new AttributeAppender("class", new Model<>("dropdown-toggle"), " "));

					contextMenu = new SubMenuPanel("subMenu", gebruikerMenuItem.getSubMenuItems());
				}
				else
				{
					caret.setVisible(false);
					contextMenu = new EmptyPanel("subMenu");
					contextMenu.setVisible(false);
					link = new Link<Object>("link")
					{
						@Override
						public void onClick()
						{
							setResponsePage(GebruikerMenuItem.getTargetPageClass(gebruikerMenuItem));
						}
					};
				}

				item.add(container);
				container.add(contextMenu);
				container.add(link);

				link.add(status);
				link.add(caret);
				link.add(new Label("naam", new SimpleStringResourceModel(item.getModelObject().getResourceTag())));
			}

		};
		hoofdmenu.setVisible(CollectionUtils.isNotEmpty(getHoofdMenuItems()));
		add(hoofdmenu);
	}

	private List<GebruikerMenuItem> getHoofdMenuItems()
	{
		List<GebruikerMenuItem> result = new ArrayList<>();

		for (GebruikerHoofdMenuItem hoofdMenuItem : GebruikerHoofdMenuItem.values())
		{
			GebruikerMenuItem menuItem = hoofdMenuItem.getMenuItem();

			Class<? extends GebruikerBasePage> targetPage = GebruikerMenuItem.getTargetPageClass(menuItem);
			if (targetPage != null && Session.get().getAuthorizationStrategy().isInstantiationAuthorized(targetPage))
			{
				result.add(menuItem);
			}
		}

		return result;
	}

	protected abstract GebruikerHoofdMenuItem getActieveMenuItem();

	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return new ArrayList<>();
	}

	public List<GebruikerMenuItem> getAllowedContextMenuItems()
	{
		List<GebruikerMenuItem> menuItems = new ArrayList<>();
		List<GebruikerMenuItem> contextMenuItems = getContextMenuItems();
		if (CollectionUtils.isNotEmpty(contextMenuItems))
		{
			for (GebruikerMenuItem contextMenuItem : contextMenuItems)
			{
				Class<? extends GebruikerBasePage> targetPageClass = contextMenuItem.getTargetPageClass();
				if (targetPageClass != null && Session.get().getAuthorizationStrategy().isInstantiationAuthorized(targetPageClass))
				{
					menuItems.add(contextMenuItem);
				}
			}
		}

		return menuItems;
	}

	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return getClass();
	}

	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return getClass();
	}

	@Override
	public ScreenitContext getContext()
	{
		return ScreenitContext.GEBRUIKER;
	}

	protected boolean isMinimumActie(Actie actie, Actie minimaal)
	{
		return Actie.INZIEN.equals(minimaal) && actie == null || actie != null && actie.getNiveau() >= minimaal.getNiveau();
	}

	public void setLoginIms(boolean loginIms)
	{
		this.loginIms = loginIms;
	}

	@Override
	protected void setHeaders(WebResponse response)
	{
		if (heeftImsKoppelingRecht)
		{
			SecurityHeadersFilter.allowExtraConnectSrcInContentSecurityPolicy(response, "https://localhost:7001 wss://localhost:7002");
		}
		super.setHeaders(response);
	}

	private void afmeldenConformAjaxDialog()
	{
		add(new ConfirmingIndicatingAjaxLink<Void>("afmelden", dialog, "afmelden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				logout();
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				if (heeftImsKoppelingRecht)
				{
					GebruikerBasePage basePage = GebruikerBasePage.this;
					if (isMammaBeoordelaar() && (!(basePage instanceof AbstractMammaBePage) || (!((AbstractMammaBePage) basePage).heeftOnderzoekenInWerklijst())
						&& !((AbstractMammaBePage) basePage).heeftVerslagenTeBevestigen()))
					{
						logoutFromIms(attributes);
					}
				}
			}

			@Override
			protected boolean skipConfirmation()
			{
				return !ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker().getInlogMethode().equals(InlogMethode.UZIPAS);
			}

		});
	}

	protected void logoutFromIms(AjaxRequestAttributes attributes)
	{
		AjaxCallListener myAjaxCallListener = new AjaxCallListener();
		myAjaxCallListener.onBeforeSend(createImsLogoffCommand());
		attributes.getAjaxCallListeners().add(myAjaxCallListener);
	}

	private String createImsErrorAfhandeling()
	{
		return "imsErrorCallback = " + imsErrorCallbackBehavior.getCallbackFunction(CallbackParameter.explicit(MAMMA_IMS_ERROR_CALLBACK_PARAM_MESSAGE),
			CallbackParameter.explicit(MAMMA_IMS_ERROR_CALLBACK_PARAM_ONDERZOEK)) + ";";
	}

	private void createImsErrorCallback()
	{
		imsErrorCallbackBehavior = new AbstractDefaultAjaxBehavior()
		{
			@Override
			protected void respond(AjaxRequestTarget target)
			{
				IRequestParameters requestParameters = getComponent().getRequest().getRequestParameters();
				String errorMessage = requestParameters.getParameterValue(MAMMA_IMS_ERROR_CALLBACK_PARAM_MESSAGE).toString();
				StringValue onderzoekIdStringValue = requestParameters.getParameterValue(MAMMA_IMS_ERROR_CALLBACK_PARAM_ONDERZOEK);
				Long onderzoekId = onderzoekIdStringValue.isEmpty() ? null : Long.parseLong(onderzoekIdStringValue.toString());
				handleImsError(target, errorMessage, onderzoekId);
			}
		};
		add(imsErrorCallbackBehavior);
	}

	protected void handleImsError(AjaxRequestTarget target, String errorMessage, Long onderzoekId)
	{

	}

	protected boolean isHeeftImsKoppelingRecht()
	{
		return heeftImsKoppelingRecht;
	}

	public boolean isMammaBeoordelaar()
	{
		return isMammaBeoordelaar;
	}

	public BootstrapDialog getDialog()
	{
		return dialog;
	}
}
