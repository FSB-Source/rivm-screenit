package nl.rivm.screenit.wsb.config.cxf;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingInInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingOutInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;
import nl.rivm.screenit.wsb.pd.interceptor.ScreenITFaultListener;

import org.apache.cxf.Bus;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@ImportResource({ "classpath:META-INF/cxf/cxf.xml", "classpath:META-INF/cxf/cxf-servlet.xml" })
@Order(Ordered.HIGHEST_PRECEDENCE)
@AllArgsConstructor
public class CxfConfig
{

	private final ApplicationContext applicationContext;

	private final ScreenITFaultListener faultListener;

	@Bean
	public Bus wsbBus()
	{
		var loggingInInterceptor = new ScreenITLoggingInInterceptor(screenITLoggingSaver());
		var loggingOutInterceptor = new ScreenITLoggingOutInterceptor(screenITLoggingSaver());

		Bus defaultBus = (Bus) applicationContext.getBean(Bus.DEFAULT_BUS_ID);
		defaultBus.getInInterceptors().add(loggingInInterceptor);
		defaultBus.getOutInterceptors().add(loggingOutInterceptor);
		defaultBus.getInFaultInterceptors().add(loggingInInterceptor);
		defaultBus.getOutFaultInterceptors().add(loggingOutInterceptor);
		defaultBus.getProperties().put("org.apache.cxf.logging.FaultListener", faultListener);
		return defaultBus;
	}

	@Bean
	public ScreenITLoggingSaver screenITLoggingSaver()
	{
		return new ScreenITLoggingSaver();
	}
}
