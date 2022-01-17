package nl.rivm.screenit.main.web.config;

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

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ImportResource(
	locations = {

		"classpath:applicationContext-hibernate5-extension-spring.xml",
		"classpath:applicationContext-spring-extension-injection.xml",
		"classpath:applicationContext-autowired.xml",
		"classpath:applicationContext-shiro.xml",
		"classpath:applicationContext-shiro-web.xml",
		"classpath:applicationContext-hibernate-base.xml",
		"classpath:applicationContext-hibernate.xml",
		"classpath:applicationContext-jms.xml",
		"classpath:applicationContext-jms-web.xml",
		"classpath:applicationContext-jndi.xml",
		"classpath:applicationContext-jndi-web.xml",
		"classpath:applicationContext-mailfrom.xml",
		"classpath:applicationContext-preference-module.xml",
		"classpath:applicationContext-wicket-planning.xml",
		"classpath:applicationContext-factories.xml",
		"classpath:applicationContext-edi.xml",
		"classpath:applicationContext-documentupload.xml",
		"classpath:applicationContext-wicket-password.xml",
		"classpath:applicationContext-csv2xls.xml",
	})
public class ImportXmlOrder
{
}
