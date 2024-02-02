package nl.rivm.screenit.main.web;

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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.function.Supplier;

import net.ftlines.wicketsource.WicketSource;

import nl.dries.wicket.hibernate.dozer.SessionFinder;
import nl.dries.wicket.hibernate.dozer.SessionFinderHolder;
import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.ZorgIdSessieService;
import nl.rivm.screenit.main.web.ScreenitSession.ScreenitSessionRevisionInformationResolverDelegate;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.LocalTimeConverter;
import nl.rivm.screenit.main.web.component.MultiDateConverter;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.TimeLocalDateConverter;
import nl.rivm.screenit.main.web.component.TimeLocalTimeConverter;
import nl.rivm.screenit.main.web.error.ErrorPage;
import nl.rivm.screenit.main.web.error.PageExpiredPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.verslag.ClientVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.ScreenitExpressieProvider;
import nl.rivm.screenit.main.web.gebruiker.login.MedewerkerLoginMethodPage;
import nl.rivm.screenit.main.web.gebruiker.login.OrganisatieSelectiePage;
import nl.rivm.screenit.main.web.gebruiker.login.PasswordChangePage;
import nl.rivm.screenit.main.web.gebruiker.login.UitwisselportaalLoginPage;
import nl.rivm.screenit.main.web.security.ScreenitAnnotationsShiroAuthorizationStrategy;
import nl.rivm.screenit.main.web.security.ScreenitShiroUnauthorizedComponentListener;
import nl.rivm.screenit.model.envers.RevisionInformationResolver;
import nl.rivm.screenit.model.envers.RevisionKenmerkInThreadHolder;
import nl.topicuszorg.cloud.distributedsessions.RedisConfig;
import nl.topicuszorg.cloud.distributedsessions.wicket.RedisPageManagerProvider;
import nl.topicuszorg.formulieren2.expressie.ExpressieSettings;
import nl.topicuszorg.formulieren2.expressie.definitie.IExpressieProvider;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.wicket.hibernate.util.EntityAndSerializableCheckerListener;
import nl.topicuszorg.wicket.input.converters.DoubleConverter;

import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.ConverterLocator;
import org.apache.wicket.DefaultExceptionMapper;
import org.apache.wicket.IConverterLocator;
import org.apache.wicket.Page;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.AjaxRequestTarget.IJavaScriptResponse;
import org.apache.wicket.ajax.AjaxRequestTarget.IListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.coop.CrossOriginOpenerPolicyConfiguration;
import org.apache.wicket.core.request.handler.PageProvider;
import org.apache.wicket.core.util.crypt.KeyInSessionSunJceCryptFactory;
import org.apache.wicket.csp.CSPDirective;
import org.apache.wicket.csp.CSPDirectiveSrcValue;
import org.apache.wicket.extensions.ajax.markup.html.AjaxLazyLoadPanel;
import org.apache.wicket.feedback.FeedbackMessages;
import org.apache.wicket.markup.html.pages.ExceptionErrorPage;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.request.IExceptionMapper;
import org.apache.wicket.request.IRequestHandler;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.Response;
import org.apache.wicket.request.http.handler.ErrorCodeRequestHandler;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.apache.wicket.resource.JQueryResourceReference;
import org.apache.wicket.session.HttpSessionStore;
import org.apache.wicket.settings.ExceptionSettings;
import org.apache.wicket.settings.RequestLoggerSettings;
import org.apache.wicket.spring.injection.annot.SpringComponentInjector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.googlecode.wicket.jquery.ui.settings.JQueryUILibrarySettings;

@org.springframework.stereotype.Component
public class ScreenitApplication extends WebApplication
{
	private static final Logger LOG = LoggerFactory.getLogger(ScreenitApplication.class);

	public static final String UITWISSELPORTAAL_MOUNT = "uitwisselportaal";

	private static String sessionAttributePrefix;

	private String versionString;

	private RedisConfig redisConfig;

	@Override
	protected void init()
	{
		super.init();

		RevisionInformationResolver.registerDelegate(new ScreenitSessionRevisionInformationResolverDelegate());
		SessionFinderHolder.setSessionFinder(ApplicationContextProvider.getApplicationContext().getBean(SessionFinder.class));
		sessionAttributePrefix = getSessionAttributePrefix(null, null);

		readVersie();
		initSpring();

		getMarkupSettings().setStripWicketTags(true);
		getPageSettings().setRecreateBookmarkablePagesAfterExpiry(false);

		getRequestCycleListeners().add(new ScreenITCsrfRequestCycleListener());

		getCspSettings().blocking().strict().remove(CSPDirective.DEFAULT_SRC)
			.add(CSPDirective.DEFAULT_SRC, CSPDirectiveSrcValue.SELF)
			.add(CSPDirective.OBJECT_SRC, CSPDirectiveSrcValue.SELF)
			.add(CSPDirective.STYLE_SRC, CSPDirectiveSrcValue.SELF)
			.add(CSPDirective.FONT_SRC, CSPDirectiveSrcValue.SELF)
			.add(CSPDirective.FRAME_ANCESTORS, CSPDirectiveSrcValue.SELF)
			.add(CSPDirective.IMG_SRC, "data:")
			.add(CSPDirective.CONNECT_SRC, "https://browser-intake-datadoghq.eu");
		getSecuritySettings().setCrossOriginOpenerPolicyConfiguration(CrossOriginOpenerPolicyConfiguration.CoopMode.SAME_ORIGIN_ALLOW_POPUPS);

		ScreenitAnnotationsShiroAuthorizationStrategy authz = new ScreenitAnnotationsShiroAuthorizationStrategy();
		getSecuritySettings().setAuthorizationStrategy(authz);
		getSecuritySettings().setUnauthorizedComponentInstantiationListener(new ScreenitShiroUnauthorizedComponentListener(null, null, authz));
		getApplicationSettings().setInternalErrorPage(ErrorPage.class);
		getApplicationSettings().setPageExpiredErrorPage(PageExpiredPage.class);
		getExceptionSettings().setUnexpectedExceptionDisplay(ExceptionSettings.SHOW_INTERNAL_ERROR_PAGE);

		getSecuritySettings().setCryptFactory(new KeyInSessionSunJceCryptFactory());

		mountPage("medewerkerportaal", MedewerkerLoginMethodPage.class);
		mountPage("passwordchange", PasswordChangePage.class);
		mountPage(UITWISSELPORTAAL_MOUNT, UitwisselportaalLoginPage.class);

		getAjaxRequestTargetListeners().add(new IListener()
		{
			@Override
			public void onBeforeRespond(Map<String, Component> map, AjaxRequestTarget target)
			{
				if (target.getPage() instanceof BasePage)
				{
					BasePage basePage = (BasePage) target.getPage();
					if (map != null && map.size() == 1 && map.values().toArray()[0] instanceof AjaxLazyLoadPanel)
					{
						boolean sessionFeedbackMessages = !Session.get().getFeedbackMessages().isEmpty();
						boolean ajaxLazyLoadPanelFeedbackMessages = !((AjaxLazyLoadPanel<?>) map.values().toArray()[0]).getFeedbackMessages().isEmpty();
						List<FeedbackMessages> childsFeedbackMessages = new ArrayList<>();

						((AjaxLazyLoadPanel<?>) map.values().toArray()[0]).visitChildren((component, iVisit) ->
						{
							if (!component.getFeedbackMessages().isEmpty())
							{
								childsFeedbackMessages.add(component.getFeedbackMessages());
							}
						});

						if (sessionFeedbackMessages || ajaxLazyLoadPanelFeedbackMessages || !childsFeedbackMessages.isEmpty())
						{
							basePage.refreshFeedback(target);
						}
					}
					else if (!isTimerRequest(target))
					{
						basePage.refreshFeedback(target);
						ApplicationContextProvider.getApplicationContext().getBean(ZorgIdSessieService.class).refreshSessie(ScreenitSession.get().getZorgIdSession().getUuid());
					}
					if (basePage instanceof ClientVerslagPage)
					{
						target.appendJavaScript("initNullFlavourFields()");
					}
					target.appendJavaScript("laadEventListeners()");
				}
			}

			private boolean isTimerRequest(AjaxRequestTarget target)
			{
				Set<String> parameterNames = target.getPage().getRequest().getRequestParameters().getParameterNames();
				return parameterNames.contains(PollingAbstractAjaxTimerBehavior.MARKER);
			}

			@Override
			public void onAfterRespond(Map<String, Component> map, IJavaScriptResponse response)
			{
				IExpressieProvider expressieProvider = ExpressieSettings.getExpressieProvider();
				if (expressieProvider instanceof ScreenitExpressieProvider)
				{
					((ScreenitExpressieProvider) expressieProvider).resetCache();
				}
				RevisionKenmerkInThreadHolder.resetKenmerk();
			}

			@Override
			public void updateAjaxAttributes(AbstractDefaultAjaxBehavior abstractDefaultAjaxBehavior, AjaxRequestAttributes ajaxRequestAttributes)
			{
			}
		});

		if (RuntimeConfigurationType.DEPLOYMENT != getConfigurationType())
		{
			getRequestCycleListeners().add(new EntityAndSerializableCheckerListener(new Class[] { OrganisatieSelectiePage.class }, true));
			WicketSource.configure(this);
		}

		getJavaScriptLibrarySettings().setJQueryReference(JQueryResourceReference.getV3());

		JQueryUILibrarySettings jQueryUILibrarySettings = JQueryUILibrarySettings.get();
		jQueryUILibrarySettings.setJavaScriptReference(new JavaScriptResourceReference(ScreenitApplication.class, "jquery-ui-1.10.3.js"));

		initRequestLogger();

		initDistributedJettySessions();
	}

	protected void initSpring()
	{
		getComponentInstantiationListeners().add(new SpringComponentInjector(this));
	}

	protected void readVersie()
	{
		StringBuilder versieBuilder = new StringBuilder();
		Properties applicationProperties = new Properties();
		try (InputStream resourceAsStream = this.getClass().getResourceAsStream("/build-info.properties"))
		{
			applicationProperties.load(resourceAsStream);
			String version = applicationProperties.getProperty("build.version");
			String timestamp = applicationProperties.getProperty("build.time");
			versieBuilder.append(version);
			if ("SNAPSHOT".equals(version))
			{
				String buildnumber = applicationProperties.getProperty("build.number");

				if (!"${BUILD_NUMBER}".equals(buildnumber))
				{
					versieBuilder.append("-").append(buildnumber);
				}

				LOG.info("ScreenIT build #{}", buildnumber);
			}
			else
			{
				LOG.info("ScreenIT versie {}", version);
			}
			LOG.info("Build datum: {}", timestamp);

		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden van build-info.properties (voor versienummer)");
		}
		versionString = versieBuilder.toString();
	}

	private void initRequestLogger()
	{
		RequestLoggerSettings reqLogger = getRequestLoggerSettings();
		reqLogger.setRequestLoggerEnabled(true);
		reqLogger.setRecordSessionSize(false);
		reqLogger.setRequestsWindowSize(0);
	}

	public String getVersionString()
	{
		return versionString;
	}

	@Override
	public Class<? extends Page> getHomePage()
	{
		if (ScreenitSession.get().isFromUitwisselportaal())
		{
			return UitwisselportaalLoginPage.class;
		}
		else
		{
			return MedewerkerLoginMethodPage.class;
		}
	}

	@Override
	public Session newSession(Request request, Response response)
	{
		ScreenitSession screenitSession = new ScreenitSession(request);
		screenitSession.setLocale(Constants.LOCALE_NL);
		Locale.setDefault(Constants.LOCALE_NL);
		return screenitSession;
	}

	public static ScreenitApplication get()
	{
		return (ScreenitApplication) Application.get();
	}

	@Override
	protected IConverterLocator newConverterLocator()
	{
		ConverterLocator converterLocator = (ConverterLocator) super.newConverterLocator();
		MultiDateConverter datumConverter = new MultiDateConverter();
		converterLocator.set(java.util.Date.class, datumConverter);
		converterLocator.set(java.sql.Date.class, datumConverter);
		converterLocator.set(java.sql.Timestamp.class, datumConverter);
		converterLocator.set(java.util.Calendar.class, datumConverter);
		converterLocator.set(org.joda.time.LocalTime.class, new LocalTimeConverter());
		converterLocator.set(java.time.LocalDate.class, new TimeLocalDateConverter());
		converterLocator.set(java.time.LocalTime.class, new TimeLocalTimeConverter());
		converterLocator.set(Double.TYPE, new DoubleConverter());
		converterLocator.set(Double.class, new DoubleConverter());

		return converterLocator;
	}

	@Override
	public final Supplier<IExceptionMapper> getExceptionMapperProvider()
	{
		return () -> new DefaultExceptionMapper()
		{

			@Override
			protected IRequestHandler mapUnexpectedExceptions(Exception e, Application application)
			{
				final ExceptionSettings.UnexpectedExceptionDisplay unexpectedExceptionDisplay = application.getExceptionSettings()
					.getUnexpectedExceptionDisplay();

				LOG.error("Unexpected error occurred", e);

				if (ExceptionSettings.SHOW_EXCEPTION_PAGE.equals(unexpectedExceptionDisplay))
				{
					Page currentPage = extractCurrentPage();
					return createPageRequestHandler(new PageProvider(new ExceptionErrorPage(e,
						currentPage)));
				}
				else if (ExceptionSettings.SHOW_INTERNAL_ERROR_PAGE.equals(unexpectedExceptionDisplay))
				{
					return createPageRequestHandler(new PageProvider(application.getApplicationSettings().getInternalErrorPage()));

				}

				return new ErrorCodeRequestHandler(500);
			}

		};
	}

	public static String getSessionAttributePrefix()
	{
		return sessionAttributePrefix;
	}

	private void initDistributedJettySessions()
	{
		if (!getRedisConfig().isEnabled())
		{
			LOG.warn("Gedistribueerde sessies zijn uitgeschakeld!");
			return;
		}

		LOG.info("Gedistribueerde sessies zijn ingeschakeld");

		setSessionStoreProvider(HttpSessionStore::new);

		setPageManagerProvider(new RedisPageManagerProvider(this, getRedisConfig()));
		ApplicationContextProvider.getApplicationContext().getBean(ZorgIdSessieService.class).setRedisConfig(getRedisConfig());
	}

	public RedisConfig getRedisConfig()
	{
		if (redisConfig == null)
		{
			redisConfig = new RedisConfig();
		}
		return redisConfig;
	}

}
