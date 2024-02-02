package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeWerklijstService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dashboard.MammaRadioloogDashboardPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.popup.MammaLogoutConfirmationDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review.MammaReviewWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaArbitrageWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaBeoordelingenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaDiscrepantieWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaVerslagenWerklijstPage;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;

import org.apache.wicket.Component;
import org.apache.wicket.Page;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class AbstractMammaBePage extends MammaScreeningBasePage
{
	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBeWerklijstService beWerklijstService;

	private final boolean heeftToegangTotOnderzoektypeFilter;

	private final List<Component> postfixes = new ArrayList<>();

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/css/base_styles_be.css"));
	}

	protected AbstractMammaBePage()
	{
		heeftToegangTotOnderzoektypeFilter = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_BE_ONDERZOEKTYPE_FILTER, Actie.INZIEN);
		PollingAbstractAjaxTimerBehavior timer = new PollingAbstractAjaxTimerBehavior(Duration.ofSeconds(5))
		{
			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				for (Component comp : postfixes)
				{
					var parentPage = comp.findParent(Page.class);
					if (parentPage != null)
					{

						target.add(comp);
					}
				}
			}
		};
		add(timer);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.dashboard", MammaRadioloogDashboardPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.review", MammaReviewWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.beoordelen", MammaBeoordelingenWerklijstPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN, MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN));
			}
		});

		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.discrepantie", MammaDiscrepantieWerklijstPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, Collections.singletonList(MammaBeoordelingStatus.DISCREPANTIE));
			}
		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.arbitrage", MammaArbitrageWerklijstPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, Collections.singletonList(MammaBeoordelingStatus.ARBITRAGE));
			}
		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.beoordeling.verslagen", MammaVerslagenWerklijstPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, Arrays.asList(MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.VERSLAG_AFGEKEURD));
			}
		});

		return contextMenuItems;
	}

	private Component getAantalPostfixLabel(String id, List<MammaBeoordelingStatus> beoordelingStatussen)
	{
		Label label = new Label(id, new LoadableDetachableModel<String>()
		{
			@Override
			protected String load()
			{
				MammaBeWerklijstZoekObject zoekObject = new MammaBeWerklijstZoekObject();
				zoekObject.setBeoordelingStatussen(beoordelingStatussen);
				zoekObject.setBeoordelingsEenheid((BeoordelingsEenheid) ScreenitSession.get().getInstelling());

				if (!heeftToegangTotOnderzoektypeFilter)
				{
					zoekObject.setOnderzoekType(MammaOnderzoekType.MAMMOGRAFIE);
				}

				zoekObject.setInstellingGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());

				var aantalOnderzoeken = beoordelingService.countOnderzoeken(zoekObject);

				if (aantalOnderzoeken > 0L)
				{
					return "" + aantalOnderzoeken;
				}
				else
				{
					return "";
				}
			}
		});
		label.add(new AttributeAppender("class", "number-alert"));
		label.setOutputMarkupId(true);
		postfixes.add(label);
		return label;
	}

	@Override
	protected void logout()
	{
		if (isMammaBeoordelaar())
		{
			boolean heeftOnderzoekenInWerklijst = heeftOnderzoekenInWerklijst();
			boolean heeftVerslagenTeBevestigen = heeftVerslagenTeBevestigen();
			if (heeftOnderzoekenInWerklijst || heeftVerslagenTeBevestigen)
			{
				MammaLogoutConfirmationDialog mammaLogoutConfirmationDialog = maakMammaLogoutConfimationDialoog(heeftOnderzoekenInWerklijst, heeftVerslagenTeBevestigen);
				AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
				dialog.openWith(target, mammaLogoutConfirmationDialog);
				return;
			}
		}
		super.logout();
	}

	public boolean heeftOnderzoekenInWerklijst()
	{
		ScreenitSession screenitSession = ScreenitSession.get();
		InstellingGebruiker instellingGebruiker = screenitSession.getLoggedInInstellingGebruiker();
		BeoordelingsEenheid beoordelingsEenheid = (BeoordelingsEenheid) screenitSession.getInstelling();

		return beWerklijstService.heeftOnderzoekenInWerklijst(instellingGebruiker, beoordelingsEenheid);
	}

	public boolean heeftVerslagenTeBevestigen()
	{
		ScreenitSession screenitSession = ScreenitSession.get();
		InstellingGebruiker instellingGebruiker = screenitSession.getLoggedInInstellingGebruiker();

		return beoordelingService.is1eOf2eLezingenTeBevestigen(instellingGebruiker);
	}

	private MammaLogoutConfirmationDialog maakMammaLogoutConfimationDialoog(boolean heeftOnderzoekenInWerklijst, boolean heeftVerslagenTeBevestigen)
	{
		return new MammaLogoutConfirmationDialog(IDialog.CONTENT_ID, heeftVerslagenTeBevestigen, heeftOnderzoekenInWerklijst)
		{
			@Override
			protected void close(AjaxRequestTarget target, boolean logout)
			{
				dialog.close(target);
				if (logout)
				{
					AbstractMammaBePage.super.logout();
				}
				else
				{
					onCloseLogoutConfirmationDialog(target);
				}
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				if (isHeeftImsKoppelingRecht())
				{
					AbstractMammaBePage.super.logoutFromIms(attributes);
				}
			}
		};
	}

	protected void onCloseLogoutConfirmationDialog(AjaxRequestTarget target)
	{
		setResponsePage(MammaBeoordelingenWerklijstPage.class);
	}
}
